
module IntermediateLang.ILTypes where

import Language.C.Syntax.AST(CStatement,
                             CDeclaration,
                             CStatement(CCompound),
                             CDecl,
                             CDeclr,
                             CDeclaration(CDecl),
                             CDeclarator(CDeclr),
                             CDerivedDeclarator(CPtrDeclr,
                                                CFunDeclr,
                                                CArrDeclr),
                             CTypeSpec,
                             CArraySize(CNoArrSize),
                             CDeclarationSpecifier(CTypeSpec,CTypeQual),
                             CTypeSpecifier(CCharType,
                                            CVoidType,
                                            CShortType,
                                            CDoubleType,
                                            CSUType,
                                            CIntType,
                                            CUnsigType,
                                            CSignedType,
                                            CLongType,
                                            CEnumType,
                                            CTypeDef,
                                            CFloatType),
                             CTypeQualifier(CConstQual),
                             CStructTag(CStructTag,CUnionTag),
                             CEnumeration(CEnum),
                             CStructureUnion(CStruct),
                             CCompoundBlockItem(CBlockDecl))
import Language.C.Data.Ident(Ident,identToString,internalIdent)
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Pretty

data ILBaseType = ILVoid
                | ILChar TypeSign
                | ILShort TypeSign
                | ILInt TypeSign
                | ILLong TypeSign
                | ILFloat TypeSign
                | ILDouble TypeSign
                | ILSizeOf
                | ILStructRef ILTypeName
                | ILUnionRef ILTypeName
                | ILEnumRef ILTypeName
                | ILNewName ILTypeName
                deriving (Eq,Ord,Show)

data TypeSign = Signed | Unsigned | NoSign
              deriving (Eq,Ord,Show)

data Constness = Const | NotConst
               deriving (Eq,Ord,Show)

newtype ILTypeName = ILTypeName
                     {fromILTypeName :: String}
                   deriving (Eq,Ord,Show)

data ILType = FuncType ILType [ILType]
            | BaseType Constness ILBaseType
            | PtrType Constness ILType
            | ArrType ILType 
            deriving (Eq,Ord,Show)

makeDeclrList name =
  let nameDeclr = internalIdent name
      declrs = CDeclr (Just nameDeclr) [] Nothing [] undefNode in
  [(Just declrs,Nothing,Nothing)]
  

addSpecToDeclr :: CDeclaration NodeInfo ->
                  CDerivedDeclarator NodeInfo ->
                  CDeclaration NodeInfo
addSpecToDeclr (CDecl ty [(Just (CDeclr ident declrs _ _ _),_,_)] info) newDeclr =
  let declr = CDeclr ident (newDeclr : declrs) Nothing [] undefNode in
  CDecl ty [(Just declr,Nothing,Nothing)] undefNode

writeFuncDecl :: String -> ILType -> [(String,ILType)] -> CDeclaration NodeInfo
writeFuncDecl fname retType args =
  let argDeclrs = map (uncurry writeDecl) args in
  addSpecToDeclr (writeDecl fname retType) $
  CFunDeclr (Right (argDeclrs,False)) [] undefNode

writeDecl :: String -> ILType -> CDeclaration NodeInfo
writeDecl name (BaseType constness ty) =
  CDecl (typeToTypeSpecs constness ty) (makeDeclrList name) undefNode
writeDecl name (PtrType constness ty) =
  let constQual = case constness of
        Const -> [CConstQual undefNode]
        NotConst -> []
  in
  addSpecToDeclr (writeDecl name ty) (CPtrDeclr constQual undefNode)
writeDecl name (ArrType ty) =
  addSpecToDeclr (writeDecl name ty) (CArrDeclr [] (CNoArrSize False) undefNode)
writeDecl name (FuncType retType argTypes) =
  let argDeclrs = map (writeDecl "") argTypes in
  addSpecToDeclr (writeDecl name retType) $
  CFunDeclr (Right (argDeclrs,False)) [] undefNode

getDeclFromStmt :: CStatement a -> CDeclaration a
getDeclFromStmt (CCompound _ [CBlockDecl decl] _) = decl

makeConst :: ILType -> ILType
makeConst (BaseType _ base) = (BaseType Const base)
makeConst (PtrType _ base) = (PtrType Const base)

readType :: CDeclaration a -> ILType
readType (CDecl specs [] _) = typeSpecsToType specs
readType (CDecl specs [(Just (CDeclr _ declrs _ _ _),_,_)] _) =
  foldr procDeclr (typeSpecsToType specs) declrs

procDeclr :: CDerivedDeclarator a -> ILType -> ILType
procDeclr (CPtrDeclr opts _) t =
  case filter isConstQual opts of
    (_:_) -> PtrType Const t
    _ -> PtrType NotConst t
procDeclr (CArrDeclr opts _ _) t =
  let eltType  = case filter isConstQual opts of
        (_:_) -> makeConst t
        [] -> t in
  ArrType t
procDeclr (CFunDeclr (Right (args,_)) _ _) t =
  FuncType t (map readType args)

toIdent :: ILTypeName -> Ident
toIdent = internalIdent . fromILTypeName

typeToTypeSpecs :: Constness -> ILBaseType -> [CDeclarationSpecifier NodeInfo]
typeToTypeSpecs constness ty = signElt ++ constElt ++ [CTypeSpec $ fst typeElt]
  where signElt = case snd typeElt of
          Signed -> [CTypeSpec (CSignedType undefNode)]
          Unsigned -> [CTypeSpec (CUnsigType undefNode)]
          NoSign -> []
        constElt = case constness of
          Const -> [CTypeQual (CConstQual undefNode)]
          NotConst -> []
        typeElt = case ty of
          ILVoid -> (CVoidType undefNode,NoSign)
          ILChar sign -> (CCharType undefNode,sign)
          ILShort sign -> (CShortType undefNode,sign)
          ILInt sign -> (CIntType undefNode,sign)
          ILLong sign -> (CLongType undefNode,sign)
          ILFloat sign -> (CFloatType undefNode,sign)
          ILDouble sign -> (CDoubleType undefNode,sign)
          (ILStructRef tyName) ->
            (CSUType (CStruct CStructTag (Just $ toIdent tyName) Nothing [] undefNode) undefNode,
             NoSign)
          (ILUnionRef tyName) ->
            (CSUType (CStruct CUnionTag (Just $ toIdent tyName) Nothing [] undefNode) undefNode,
             NoSign)
          --FIXME Add enumRef
          (ILNewName tyName) ->
            (CTypeDef (toIdent tyName) undefNode,
             NoSign)


typeSpecsToType :: [CDeclarationSpecifier a] -> ILType
typeSpecsToType specs = BaseType constness rootType
  where rootType = getRootSpec sign specs
        constness = getConstness specs
        sign = getSignedness specs        

identToTypeName :: Ident -> ILTypeName
identToTypeName = ILTypeName . identToString

getRootSpec :: TypeSign -> [CDeclarationSpecifier a] -> ILBaseType
getRootSpec sign [] = error "No type specifier in declaration"
getRootSpec sign ((CTypeSpec (CVoidType _)):_) = ILVoid
getRootSpec sign ((CTypeSpec (CCharType _)):_) = ILChar sign
getRootSpec sign ((CTypeSpec (CShortType _)):_) = ILShort sign
getRootSpec sign ((CTypeSpec (CIntType _)):_) = ILInt sign
getRootSpec sign ((CTypeSpec (CLongType _)):_) = ILLong sign
getRootSpec sign ((CTypeSpec (CFloatType _)):_) = ILFloat sign
getRootSpec sign ((CTypeSpec (CDoubleType _)):_) = ILDouble sign
getRootSpec sign ((CTypeSpec (CSUType (CStruct CStructTag (Just ident) _ _ _) _)):_) =
  ILStructRef $ identToTypeName ident
getRootSpec sign ((CTypeSpec (CSUType (CStruct CUnionTag (Just ident) _ _ _) _)):_) =
  ILUnionRef $ identToTypeName ident
getRootSpec sign ((CTypeSpec (CEnumType (CEnum (Just ident) _ _ _) _)):_) =
  ILEnumRef $ identToTypeName ident
getRootSpec sign ((CTypeSpec (CTypeDef ident _)):_) =
  ILNewName $ identToTypeName ident
getRootSpec sign (_:xs) = getRootSpec sign xs

isConstQual :: CTypeQualifier a -> Bool
isConstQual (CConstQual _) = True
isConstQual _ = False

getConstness :: [CDeclarationSpecifier a] -> Constness
getConstness [] = NotConst
getConstness (CTypeQual (CConstQual _):xs) =  Const
getConstness (_:xs) = getConstness xs

getSignedness :: [CDeclarationSpecifier a] -> TypeSign
getSignedness [] = NoSign
getSignedness (CTypeSpec (CUnsigType _):xs) =  Unsigned
getSignedness (CTypeSpec (CSignedType _):xs) =  Signed
getSignedness (_:xs) = getSignedness xs

