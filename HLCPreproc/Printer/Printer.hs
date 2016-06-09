{-# LANGUAGE RecordWildCards #-}


module Printer.Printer where

import AST.AST

import Text.PrettyPrint

x =
  Function (TyCon "Hey")
  [TyVar "a1"]
  []
  (DefTy (TyCon "HLCChar") [])
  [IndefTy $ TyVar "a1"]
  [VarSymb "arg1"]
  [Return (HaskFunc (VarSymb "fromIntType") [Ident $ VarSymb "arg1"])]

y =
  Struct (TyCon "MyStruct")
  [TyVar "a1"]
  [makePassable $ IndefTy $ TyVar "a1"]
  [Field IsPassable (IndefTy $ TyVar "a1") (TyCon "FieldA"),
   Field IsPassable (DefTy (TyCon "HLCChar") []) (TyCon "FieldB")]
  [VarDecl (VarSymb "fieldA") (HaskFunc (VarSymb "makeStructField") [ProxyExpr $ DefTy (TyCon "FieldA") []]),
   VarDecl (VarSymb "fieldB") (HaskFunc (VarSymb "makeStructField") [ProxyExpr $ DefTy (TyCon "FieldB") []]),
   ConsRet]
  [ConsRet]


makeTypeable :: SomeType -> TyConstraint
makeTypeable = TyConstraint (TyClass "Typeable")

makeHLCTypeable :: SomeType -> TyConstraint
makeHLCTypeable = TyConstraint (TyClass "HLCTypeable")

makePassable :: SomeType -> TyConstraint
makePassable var = TyEqual (TyFamily "Passability" var) (DefTy (TyCon "IsPassable") [])

makeTypedExpr :: SomeType -> SomeType
makeTypedExpr arg = DefTy (TyCon "TypedExpr") [arg]

instance Printable Expr where
  pp = printExpr

printExpr :: Expr -> Doc
printExpr (LitExpr lit) = pp lit
printExpr (Ident name) = pp name
printExpr (HaskFunc func args) = parens (pp func <+> (hcat $ map pp args))
printExpr (Void) = empty
printExpr (ProxyExpr ty) = parens (text "Proxy :: Proxy" <+> pp ty)
printExpr (BinOpExpr op lhs rhs) = parens (pp lhs <+> pp op <+> pp rhs)
--printExpr (UnaryOpExpr op expr) = 

printStmts :: [DoStmt] -> Doc
printStmts [] = empty
printStmts (Assignment lhs rhs:xs) =
  text "assignVar" <+> pp lhs <+> pp rhs $+$
  printStmts xs
printStmts (VarDecl symb expr:xs) =
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
  pp (ProxyExpr $ DefTy fname tyargs) <+>
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

  
printPassabilityConstraints :: [Field] -> [Doc]
printPassabilityConstraints [] = []
printPassabilityConstraints ((Field NotPassable _ _):xs) = printPassabilityConstraints xs
printPassabilityConstraints ((Field IsPassable ty _):xs) =
  (pp $ makePassable ty) :
  printPassabilityConstraints xs

printStructArgConstraints :: Struct -> Doc
printStructArgConstraints (Struct {..}) =
  parens (hcat $ punctuate comma (map (pp . makeHLCTypeable) (map IndefTy structTyParams) ++
                                  printPassabilityConstraints fields))

printStructName :: Struct -> Doc
printStructName (Struct {..}) =
  pp structName <+> hsep (map pp structTyParams)

printStruct :: Struct -> Doc
printStruct wholeStruct@(Struct {..}) =
  text "data" <+> printStructName wholeStruct $+$
  text "deriving instance" <+>
  parens (hcat $ punctuate comma $ map (pp . makeTypeable) (map IndefTy structTyParams)) <+>
  text "=>" <+> pp (DefTy (TyCon "Typeable") []) <+>
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
  where passability = case all isPassable fields of
          True -> "IsPassable"
          False -> "NotPassable"

isPassable :: Field -> Bool
isPassable (Field t _ _) = t == IsPassable

printStructFieldClassInst wholeStruct (Field passable ty name) =
  text "instance" <+>
  printStructArgConstraints wholeStruct <+>
  text "=> StructFieldClass" <+> pp passable <+>
  parens (printStructName wholeStruct) <+>
  pp name <+> pp ty

declFieldName :: Field -> Doc
declFieldName (Field passable ty name) =
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


  
