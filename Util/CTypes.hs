
module Util.CTypes where

import Language.C.Syntax.AST
import Language.C.Data.Ident

data CBaseType = CVoid
               | CChar
               | CShort
               | CInt
               | CLong
               | CFloat
               | CDouble
               | CStructRef NewTypeName
               | CUnionRef NewTypeName
               | CEnumRef NewTypeName
               | CNewName NewTypeName
               deriving (Eq,Ord,Show)

data TypeSign = Signed | Unsigned | NoSign
              deriving (Eq,Ord,Show)

data Constness = Const | NotConst
               deriving (Eq,Ord,Show)

newtype NewTypeName = NewTypeName
                      {fromNewTypeName :: String}
                    deriving (Eq,Ord,Show)

data CFullType = FuncType CFullType [CFullType]
               | BaseType TypeSign Constness CBaseType
               | PtrType Constness CFullType
               | ArrType CFullType 
               deriving (Eq,Ord,Show)

getDeclFromStmt :: CStatement a -> CDeclaration a
getDeclFromStmt (CCompound _ [CBlockDecl decl] _) = decl

makeConst :: CFullType -> CFullType
makeConst (BaseType sign _ base) = (BaseType sign Const base)
makeConst (PtrType _ base) = (PtrType Const base)

readType :: CDeclaration a -> CFullType
readType (CDecl specs [] _) = typeSpecsToType specs
readType (CDecl specs [(Just (CDeclr _ declrs _ _ _),_,_)] _) =
  foldr procDeclr (typeSpecsToType specs) declrs

procDeclr :: CDerivedDeclarator a -> CFullType -> CFullType
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

typeSpecsToType :: [CDeclarationSpecifier a] -> CFullType
typeSpecsToType specs = BaseType sign constness rootType
  where rootType = getRootSpec specs
        constness = getConstness specs
        sign = getSignedness specs        

identToTypeName :: Ident -> NewTypeName
identToTypeName = NewTypeName . identToString

getRootSpec :: [CDeclarationSpecifier a] -> CBaseType
getRootSpec [] = error "No type specifier in declaration"
getRootSpec ((CTypeSpec (CVoidType _)):_) = CVoid
getRootSpec ((CTypeSpec (CCharType _)):_) = CChar
getRootSpec ((CTypeSpec (CShortType _)):_) = CShort
getRootSpec ((CTypeSpec (CIntType _)):_) = CInt
getRootSpec ((CTypeSpec (CLongType _)):_) = CLong
getRootSpec ((CTypeSpec (CFloatType _)):_) = CFloat
getRootSpec ((CTypeSpec (CDoubleType _)):_) = CDouble
getRootSpec ((CTypeSpec (CSUType (CStruct CStructTag (Just ident) _ _ _) _)):_) =
  CStructRef $ identToTypeName ident
getRootSpec ((CTypeSpec (CSUType (CStruct CUnionTag (Just ident) _ _ _) _)):_) =
  CUnionRef $ identToTypeName ident
getRootSpec ((CTypeSpec (CEnumType (CEnum (Just ident) _ _ _) _)):_) =
  CEnumRef $ identToTypeName ident
getRootSpec ((CTypeSpec (CTypeDef ident _)):_) =
  CNewName $ identToTypeName ident
getRootSpec (_:xs) = getRootSpec xs

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

