{-# LANGUAGE RecordWildCards #-}

module Printer.Printer where

import HighLevelC.HLCTypes
import IntermediateLang.ILTypes

import Data.Sequence((><))
import Data.Foldable

import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node

import Data.Maybe

import Util.Names

e = undefNode

printCWriter :: CWriter -> CTranslationUnit NodeInfo
printCWriter (CWriter {..}) = CTranslUnit (toList decls) e
  where decls =
          fmap printStructProto structProtos ><
          fmap printFunctionProto functionProtos ><
          fmap printStructDef structDefs ><
          fmap printFuncDef funcDefs

printStructProto :: StructProto -> CExternalDeclaration NodeInfo
printStructProto (StructProto sym) =
  CDeclExt (CDecl [CTypeSpec (CSUType (CStruct CStructTag
                                       (Just $ extractExactSymbol sym)
                                       Nothing [] e) e)]
            [] e)

printFunctionProto :: FunctionProto -> CExternalDeclaration NodeInfo
printFunctionProto (FunctionProto retType name args) =
  CDeclExt $ addSpecToDeclr (writeDecl (exactSymbolName name) retType) $
  CFunDeclr (Right ((map printArg args),False)) [] e

writeDecl :: String -> ILType -> CDeclaration NodeInfo
writeDecl name (BaseType constness ty) =
  CDecl (typeToTypeSpecs constness ty) (makeDeclrList name) undefNode
writeDecl name (PtrType constness ty) =
  let constQual = case constness of
        Const -> [CConstQual undefNode]
        NotConst -> []
  in
  addSpecToDeclr (writeDecl name ty) (CPtrDeclr constQual undefNode)
writeDecl name (ArrType ty (Just n)) =
  addSpecToDeclr (writeDecl name ty) (CArrDeclr [] (CArrSize False nExpr) undefNode)
  where nExpr = CConst (CIntConst (CInteger n DecRepr (Flags 0)) e)
writeDecl name (ArrType ty Nothing) =
  addSpecToDeclr (writeDecl name ty) (CArrDeclr [] (CNoArrSize False) undefNode)
writeDecl name (FuncType retType argTypes) =
  let argDeclrs = map (writeDecl "") argTypes in
  addSpecToDeclr (writeDecl name retType) $
  CFunDeclr (Right (argDeclrs,False)) [] undefNode

printArg :: Argument -> CDeclaration NodeInfo
printArg (Argument name ty) = writeDecl (exactSymbolName name) ty

printStructDef :: StructDef -> CExternalDeclaration NodeInfo
printStructDef (StructDef sym fields) =
  CDeclExt (CDecl [CTypeSpec (CSUType (CStruct CStructTag
                                       (Just $ extractExactSymbol sym)
                                       (Just $ map printStructField fields) [] e) e)]
            [] e)

printStructField :: StructField -> CDeclaration NodeInfo
printStructField (StructField fieldName fieldType) = writeDecl (fromSafeName fieldName) fieldType

addSpecToFunDef :: CDeclaration NodeInfo ->
                   CDerivedDeclarator NodeInfo ->
                   CStatement NodeInfo ->
                   CFunctionDef NodeInfo
addSpecToFunDef (CDecl ty [(Just (CDeclr ident declrs _ _ _),_,_)] info) newDeclr stmt =
  let declr = CDeclr ident (newDeclr : declrs) Nothing [] undefNode in
  CFunDef ty declr [] stmt undefNode

printFuncDef :: FunctionDef -> CExternalDeclaration NodeInfo
printFuncDef (FunctionDef {..}) =
  CFDefExt $ addSpecToFunDef (writeDecl (exactSymbolName fdefName) fdefRetType)
  (CFunDeclr (Right ((map printArg fdefArguments),False)) [] e)
  (CCompound [] (printMainBlock fdefBody) e)

printMainBlock :: HLCBlock -> [CCompoundBlockItem NodeInfo]
printMainBlock block@(HLCBlock blockVars _ _) =
  printBlock [(Nothing,map variableDest blockVars)] block

takeUntil :: (Eq a) => (a -> Bool) -> [a] -> [a]
takeUntil p [] = []
takeUntil p (x:xs)
  | p x = [x]
  | otherwise = x:takeUntil p xs

printBlock :: [(Maybe HLCSymbol,[HLCBlock])] -> HLCBlock -> [CCompoundBlockItem NodeInfo]
printBlock cxtStack (HLCBlock blockVars (StatementList blockStmts) retCxt) =
  map (CBlockDecl . printVarDecl) blockVars ++
  concatMap (printBlock [] . variableCons) blockVars ++
  concatMap (printLabeledStmt cxtStack) blockStmts ++
  case retCxt of
    (NullContext expr) ->
      (concatMap (printBlock []) $ concatMap snd cxtStack) ++ [returnStmt expr]
    (SomeContext symb) ->
      (concatMap (printBlock []) $
       concatMap snd $
       takeUntil (\(s,_) -> s == Just symb) cxtStack) ++
      [symbLabel symb]
    NextLine -> []

returnStmt :: HLCExpr -> CCompoundBlockItem NodeInfo
returnStmt Void = CBlockStmt $ CReturn Nothing e
returnStmt expr = CBlockStmt $ CReturn (Just $ printExpr expr) e

gotoSymb :: HLCSymbol -> CCompoundBlockItem NodeInfo
gotoSymb symb  = CBlockStmt $ CGoto (extractExactSymbol symb) e

symbLabel :: HLCSymbol -> CCompoundBlockItem NodeInfo
symbLabel symb  = CBlockStmt $ CLabel (extractExactSymbol symb) (CExpr Nothing e) [] e

printExpr :: HLCExpr -> CExpression NodeInfo
printExpr (LHSExpr lhs) = printLHS lhs
printExpr (FunctionCall func args) = CCall (printExpr func) (map printExpr args) e
printExpr (LitExpr lit) = CConst (printLit lit)
printExpr (AccessPart struct elt) = CMember (printExpr struct) (safeNameToIdent elt) False e
printExpr (SizeOf ty) = CSizeofType (printType ty) e
printExpr (ExprBinOp binop lhs rhs) = CBinary (printBinOp binop) (printExpr lhs) (printExpr rhs) e
printExpr (HLCCast ty expr) = CCast (printType ty) (printExpr expr) e

printBinOp :: HLCBinOp -> CBinaryOp
printBinOp HLCPlus = CAddOp
printBinOp HLCMinus = CSubOp
printBinOp HLCTimes = CMulOp
printBinOp HLCDivide = CDivOp
printBinOp HLCRem = CRmdOp
printBinOp HLCLAnd = CLndOp
printBinOp HLCLOr = CLorOp
printBinOp HLCBitAnd = CAndOp
printBinOp HLCBitOr = COrOp
printBinOp HLCBitXor = CXorOp

printType :: ILType -> CDeclaration NodeInfo
printType (BaseType constness ty) = CDecl (typeToTypeSpecs constness ty) [] e
printType (PtrType constness ty) =
  let constQual = case constness of
        Const -> [CConstQual undefNode]
        NotConst -> []
  in
  addSpecToDeclr (printType ty) (CPtrDeclr constQual e)
printType (ArrType ty (Just n)) =
  addSpecToDeclr (printType ty) (CArrDeclr [] (CArrSize False nExpr) e)
  where nExpr = CConst (CIntConst (CInteger n DecRepr (Flags 0)) e)
printType (ArrType ty Nothing) =
  addSpecToDeclr (printType ty) (CArrDeclr [] (CNoArrSize False) e)
printType (FuncType retType argTypes) =
  let argDeclrs = map printType argTypes in
  addSpecToDeclr (printType retType) $
  CFunDeclr (Right (argDeclrs,False)) [] e

printLit :: HLCLit -> CConstant NodeInfo
printLit (CharLit c) = CCharConst (CChar (toEnum $ fromEnum c) False) e
printLit (IntLit n) = CIntConst (CInteger n DecRepr (Flags 0)) e
printLit (DoubleLit d) = CFloatConst (CFloat $ show d) e
printLit (StrLit str) = CStrConst (CString str False) e 

printLHS :: UntypedLHS -> CExpression NodeInfo
printLHS (LHSVar symb) = CVar (extractExactSymbol symb) e
printLHS (LHSPtr expr) = printExpr expr
printLHS (LHSDeref lhs) = CUnary CIndOp (printLHS lhs) e
printLHS (LHSDerefPlusOffset lhs offset) = CIndex (printLHS lhs) (printExpr offset) e
printLHS (LHSElement struct elt) = CMember (printLHS struct) (safeNameToIdent elt)  False e
printLHS (LHSAddrOf lhs) = CUnary CAdrOp (printLHS lhs) e

printVarDecl :: Variable -> CDeclaration NodeInfo
printVarDecl (Variable {..}) = writeDecl (exactSymbolName variableName) variableType

printLabeledStmt :: [(Maybe HLCSymbol,[HLCBlock])] -> HLCStatement -> [CCompoundBlockItem NodeInfo]
printLabeledStmt cxtStack (WhileStmt cond breakSymb contSymb body) =
  [CBlockStmt $ CWhile (printExpr cond) bodyStmt False e]
  where newStack =
          (Just contSymb,map variableDest $ blockVars body):
          (Just breakSymb,[]):
          cxtStack
        bodyStmt = CCompound [] (printBlock newStack body) e
printLabeledStmt cxtStack (IfThenElseStmt cond trueBranch falseBranch) =
  [CBlockStmt $ CIf
   (printExpr cond)
   (CCompound [] (printBlock trueStack trueBranch) e)
   (Just $ CCompound [] (printBlock falseStack falseBranch) e)
   e]
  where trueStack = (Nothing, map variableDest $ blockVars trueBranch) : cxtStack
        falseStack = (Nothing, map variableDest $ blockVars falseBranch) : cxtStack
printLabeledStmt cxtStack (IfThenElseRestStmt cond lbl trueBranch falseBranch) =
  [CBlockStmt $ CIf
   (printExpr cond)
   (CCompound [] (printBlock trueStack trueBranch) e)
   (Just $ CCompound [] (printBlock falseStack falseBranch) e)
   e,
   CBlockStmt $ CLabel (extractExactSymbol lbl) (CExpr Nothing e) [] e]
  where trueStack = (Just lbl, map variableDest $ blockVars trueBranch) : cxtStack
        falseStack = (Just lbl, map variableDest $ blockVars falseBranch) : cxtStack
printLabeledStmt cxtStack (AssignmentStmt lhs expr) =
  [CBlockStmt $ CExpr (Just $ CAssign CAssignOp (printLHS lhs) (printExpr expr) e) e]
printLabeledStmt cxtStack (ExpStmt expr) =
  [CBlockStmt $ CExpr (Just $ printExpr expr) e]
printLabeledStmt cxtStack (BlockStmt block) =
  [CBlockStmt (CCompound [] (printBlock cxtStack block ++ endSymb) e)]
  where endSymb =
          case blockRetCxt block of
            (SomeContext symb) -> [gotoSymb symb]
            _ -> []
