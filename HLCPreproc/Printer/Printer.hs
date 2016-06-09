{-# LANGUAGE RecordWildCards #-}


module Printer.Printer where

import AST.AST

import Text.PrettyPrint

x =
  Function (TyCon "Hey")
  [TyVar "a1"]
  []
  (DefTy $ TyCon "HLCChar")
  [IndefTy $ TyVar "a1"]
  [VarSymb "arg1"]
  [Return (HaskFunc (VarSymb "fromIntType") [LHSExpr $ LHSVar $ VarSymb "arg1"])]

y =
  Struct (TyCon "MyStruct")
  [TyVar "a1"]
  [makePassable $ IndefTy $ TyVar "a1"]
  [Field (IndefTy $ TyVar "a1") (TyCon "FieldA"),
   Field (DefTy $ TyCon "HLCChar") (TyCon "FieldB")]
  True
  [MonadicBind (VarSymb "fieldA") (HaskFunc (VarSymb "makeStructField") [ProxyExpr $ DefTy $ TyCon "FieldA"]),
   MonadicBind (VarSymb "fieldB") (HaskFunc (VarSymb "makeStructField") [ProxyExpr $ DefTy $ TyCon "FieldB"]),
   ConsRet]
  [ConsRet]

printExport :: Export -> Doc
printExport (Export fname tyArgs numArgs) =
  (text "_ <- call" <> (int numArgs)) <+>
  parens (text "Proxy :: Proxy" <+> parens (pp fname <+> hcat (map pp tyArgs))) <+>
  hcat (replicate numArgs (text "undefined"))


makeTypeable :: SomeType -> TyConstraint
makeTypeable = TyConstraint (TyClass "Typeable")

makeHLCTypeable :: SomeType -> TyConstraint
makeHLCTypeable = TyConstraint (TyClass "HLCTypeable")

makePassable :: SomeType -> TyConstraint
makePassable var = TyEqual (TyFamily "Passability" [var]) (DefTy $ TyCon "IsPassable")

makeTypedExpr :: SomeType -> SomeType
makeTypedExpr arg = TyConApp (DefTy $ TyCon "TypedExpr") [arg]

printLHS :: UntypedLHS -> Doc
printLHS (LHSVar var) = parens (text "TypedLHSVar" <+> pp var)
printLHS (LHSPtr ptr) = parens (text "TypedLHSPtr" <+> pp ptr)
printLHS (LHSDeref elt) = parens (text "TypedLHSDeref" <+> printLHS elt)
printLHS (LHSDerefPlusOffset elt expr) = parens (text "TypedLHSDerefPlusOffset" <+> printLHS elt <+> pp expr)
printLHS (LHSElement struct fieldname) = parens (text "TypedLHSElement" <+> printLHS struct <+> pp (ProxyExpr fieldname))
printLHS (LHSArrAt arr elt) = parens (text "TypedLHSArrAt" <+> printLHS arr <+> pp elt)
printLHS (LHSAddrOf var) = parens (text "TypedLHSAddrOf" <+> printLHS var)

instance Printable Expr where
  pp = printExpr

printExpr :: Expr -> Doc
printExpr (LitExpr lit) = pp lit
printExpr (LHSExpr expr) = printLHS expr
printExpr (HaskFunc func args) = parens (pp func <+> (hcat $ map pp args))
printExpr (Void) = empty
printExpr (ProxyExpr ty) = parens (text "Proxy :: Proxy" <+> pp ty)
printExpr (BinOpExpr op lhs rhs) = parens (pp lhs <+> pp op <+> pp rhs)

printStmts :: [DoStmt] -> Doc
printStmts [] = empty
printStmts (Assignment lhs rhs:xs) =
  text "assignVar" <+> pp lhs <+> pp rhs $+$
  printStmts xs
printStmts (MonadicBind symb expr:xs) =
  pp symb <+> text "<-" <+> pp expr $+$
  printStmts xs
printStmts (Return expr:xs) =
  pp retArg <+> pp expr $+$
  printStmts xs
printStmts (ConsRet:xs) =
  text "return" <+> pp retArg $+$
  printStmts xs
printStmts (FuncCall varname fname tyargs args:xs) =
  pp varname <+> text "<-" <+>
  (text "call" <> (int $ length args)) <+>
  pp (ProxyExpr $ TyConApp (DefTy fname) tyargs) <+>
  (hcat $ map pp args) $+$
  printStmts xs
printStmts (PrimVarDecl varname ty:xs) =
  pp varname <+> text "<-" <+>
  parens (text "makePrimVar" <+> parens (text "makeSafeName" <+> pp varname))
  <+> text ":: HLC" <+> parens (text "TypedVar" <+> pp ty) $+$
  printStmts xs
printStmts (StructVarDecl varname ty:xs) =
  pp varname <+> text "<-" <+>
  parens (text "makeLocalStruct" <+> parens (text "makeSafeName" <+> pp varname))
  <+> text ":: HLC" <+> parens (text "TypedVar" <+> pp ty) $+$
  printStmts xs
printStmts (Jump label:xs) =
  pp label $+$
  printStmts xs
printStmts (IfThenElseStmt cond ifBranch elseBranch:[]) =
  text "ifThenElse" <+> pp cond <+>
  parens (printStmts ifBranch) <+>
  parens (printStmts elseBranch)
printStmts (IfThenElseRestStmt cond label ifBranch elseBranch:rest) =
  text "ifThenElseRest" <+> pp cond <+>
  parens (text "\\" <> pp label <+> text "->" <+> printStmts ifBranch) <+>
  parens (text "\\" <> pp label <+> text "->" <+> printStmts elseBranch) <+>
  parens (printStmts rest)
printStmts (WhileStmt cond contLabel breakLabel body:rest) =
  text "whileRest" <+> pp cond <+>
  parens (text "\\" <> pp breakLabel <+> pp contLabel <+> text "->" <+> printStmts body) <+>
  parens (printStmts rest)

  
printPassabilityConstraint :: Field -> Doc
printPassabilityConstraint (Field ty _) =
  (pp $ makePassable ty)

printStructArgConstraints :: Struct -> Doc
printStructArgConstraints (Struct {..}) =
  parens (hcat $ punctuate comma (map (pp . makeHLCTypeable) (map IndefTy structTyParams) ++
                                  passConstraints))
  where passConstraints = case isPassable of
          True -> map printPassabilityConstraint fields
          False -> []

printStructName :: Struct -> Doc
printStructName (Struct {..}) =
  pp structName <+> hsep (map pp structTyParams)

printStruct :: Struct -> Doc
printStruct wholeStruct@(Struct {..}) =
  text "data" <+> printStructName wholeStruct $+$
  text "deriving instance" <+>
  parens (hcat $ punctuate comma $ map (pp . makeTypeable) (map IndefTy structTyParams)) <+>
  text "=>" <+> pp (DefTy $ TyCon "Typeable") <+>
  parens (pp structName <+> hsep (map pp structTyParams)) $+$
  text "instance" <+>
  printStructArgConstraints wholeStruct <+>
  text "=> HLCTypeable" <+>
  parens (pp structName <+> hsep (map pp structTyParams)) <+>
  text "where" $+$
  nest 2 (text "hlcType = structHLCType")$+$
  (vcat $ map declFieldName fields) $+$
  (vcat $ map (printStructFieldClassInst wholeStruct) fields) $+$
  text "instance" <+>
  printStructArgConstraints wholeStruct <+>
  text "=> Struct" <+> text passability <+>
  parens (printStructName wholeStruct) <+> text "where" $+$
  nest 2 (text "constructor _" <+> pp makeStructFieldArg <+> pp retArg <+> text "= do" $+$
          nest 2 (printStmts constructor) $+$
          text "destructor _" <+> pp retArg <+> text "= do" $+$
          nest 2 (printStmts destructor))
  where passability = case isPassable of
          True -> "IsPassable"
          False -> "NotPassable"

printStructFieldClassInst wholeStruct (Field ty name) =
  text "instance" <+>
  printStructArgConstraints wholeStruct <+>
  text "=> StructFieldClass" <+> passable <+>
  parens (printStructName wholeStruct) <+>
  pp name <+> pp ty
  where passable = case isPassable wholeStruct of
          True -> text "IsPassable"
          False -> text "NotPassable"

declFieldName :: Field -> Doc
declFieldName (Field ty name) =
  text "data" <+> pp name <+> text "deriving (Typeable)"

printFuncName :: Function -> Doc
printFuncName (Function {..}) = pp funcName <+> hsep (map pp funcTyParams)

printFunc :: Function -> Doc
printFunc wholeFunc@(Function {..}) =
  text "data" <+> printFuncName wholeFunc $+$
  text "instance" <+> parens (hcat $ punctuate comma $
                              map pp (map (makeHLCTypeable . IndefTy) funcTyParams ++
                                      funcTyConstraints)) <+>
  text "=> HLCFunction" <+>
  parens (pp funcName <+> hsep (map pp funcTyParams)) <+>
  parens (text "ArgWrap" <>
          (int $ length funcArgs) <+>
          (hsep $ map (parens . pp . makeTypedExpr) funcArgTypes)) <+>
  pp retType <+>
  text "where" $+$
  nest 2 (text "call _" <+> pp retArg <+> text "=" <+>
          (text "ArgWrap" <> (int $ length funcArgs)) <+>
          parens (text "\\" <> hsep (map pp funcArgs) <+> text "-> do" $+$
                 nest 2 (printStmts funcBody)))


  
