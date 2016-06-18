{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module PostProcess.Printer where

import HighLevelC.HLCTypes
import IntermediateLang.ILTypes

import Data.Sequence((><))
import Data.Foldable

import Quasi.QuasiC

import Language.C.Syntax.AST
import Language.C.Syntax.Constants
import Language.C.Data.Node
import Language.C.Pretty
import Language.C.Data.Ident

import Language.Haskell.TH(Name,nameBase)

import Data.Maybe

import Text.PrettyPrint

import PostProcess.SymbolRewrite
import PostProcess.ObjectRewrite

import Util.Names
import Util.THUtil

e = undefNode

mainFunction :: (NodeInfo -> CExpression NodeInfo)
             -> CExternalDeclaration NodeInfo
mainFunction callName = [cDecl|
int main(int argc, char** argv)
{
  return $callName (argc,argv);
}|]

printPreProDir :: PreprocessorDirective -> Doc
printPreProDir (PreprocessorDirective str) = text str

printWholeTU :: Maybe Name -> CWriter -> Doc
printWholeTU mainFuncName cwriter =
  (vcat $ map printPreProDir (toList $ preproDirs cwriter)) $+$
  (pretty $ printCWriter $ processObjects $ processSymbols cwriter) $+$
  maybe empty (pretty .
               mainFunction .
               (\name -> CVar (internalIdent $ capitalize $ nameBase name))) mainFuncName

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
printMainBlock block@(HLCBlock _ _ VoidReturn) =
  printBlock block ++
  [returnStmt Void]
printMainBlock block@(HLCBlock _ _ (NullContext var _)) =
  printBlock block ++
  [returnStmt $ LHSExpr $ LHSVar $ variableName var]
printMainBlock x = error (show x)

printBlock :: HLCBlock -> [CCompoundBlockItem NodeInfo]
printBlock (HLCBlock blockVars (StatementList blockStmts) _) =
  map (CBlockDecl . printVarDecl) blockVars ++
  concatMap printLabeledStmt blockStmts

returnStmt :: HLCExpr -> CCompoundBlockItem NodeInfo
returnStmt Void = CBlockStmt $ CReturn Nothing e
returnStmt expr = CBlockStmt $ CReturn (Just $ printExpr expr) e

printExpr :: HLCExpr -> CExpression NodeInfo
printExpr (LHSExpr lhs) = printLHS lhs
printExpr (FunctionCall func args) = CCall (printExpr func) (map printExpr args) e
printExpr (LitExpr lit) = CConst (printLit lit)
printExpr (AccessPart struct elt) = CMember (printExpr struct) (safeNameToIdent elt) False e
printExpr (SizeOf ty) = CSizeofType (printType ty) e
printExpr (ExprNegate subexpr) = CUnary CMinOp (printExpr subexpr) e
printExpr (ExprBinOp binop lhs rhs) = CBinary (printBinOp binop) (printExpr lhs) (printExpr rhs) e
printExpr (HLCTernary cond ifExpr elseExpr) =
  CCond (printExpr cond) (Just $ printExpr ifExpr) (printExpr elseExpr) e
printExpr (HLCCast ty expr) = CCast (printType ty) (printExpr expr) e
printExpr (Void) = error "Tried to print Void expr"

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
printBinOp HLCEqual = CEqOp
printBinOp HLCLT = CLeOp
printBinOp HLCGT = CGrOp
printBinOp HLCLTEQ = CLeqOp
printBinOp HLCGTEQ = CGeqOp
printBinOp HLCSHL = CShlOp
printBinOp HLCSHR = CShrOp

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

printLabeledStmt :: HLCStatement -> [CCompoundBlockItem NodeInfo]
printLabeledStmt (WhileStmt cond _ _ body) =
  [CBlockStmt $ CWhile (printExpr cond) (CCompound [] (printBlock body) e) False e]
printLabeledStmt (IfThenElseStmt cond trueBranch falseBranch) =
  [CBlockStmt $ CIf
   (printExpr cond)
   (CCompound [] (printBlock trueBranch) e)
   (Just $ CCompound [] (printBlock falseBranch) e)
   e]
printLabeledStmt (IfThenElseRestStmt cond _ trueBranch falseBranch) =
  [CBlockStmt $ CIf
   (printExpr cond)
   (CCompound [] (printBlock trueBranch) e)
   (Just $ CCompound [] (printBlock falseBranch) e)
   e]
printLabeledStmt (AssignmentStmt lhs expr) =
  [CBlockStmt $ CExpr (Just $ CAssign CAssignOp (printLHS lhs) (printExpr expr) e) e]
printLabeledStmt (ExpStmt expr) =
  [CBlockStmt $ CExpr (Just $ printExpr expr) e]
printLabeledStmt (BlockStmt block) =
  [CBlockStmt (CCompound [] (printBlock block) e)]
printLabeledStmt (LabelStmt symb) =
  [CBlockStmt $ CLabel (extractExactSymbol symb) (CExpr Nothing e) [] e]
printLabeledStmt (JumpStmt symb) =
  [CBlockStmt $ CGoto (extractExactSymbol symb) e]

