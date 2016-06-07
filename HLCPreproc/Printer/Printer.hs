{-# LANGUAGE RecordWildCards #-}


module Printer.Printer where

import AST.AST

import Text.PrettyPrint

x =
  Function (DefiniteType "Hey")
  [TyVar "a1"]
  []
  (TyArgSymb $ DefTy $ DefiniteType "HLCChar")
  [TyArgVar $ TyVar "a1"]
  [VarSymb "arg1"]
  (ContSymb "ret")
  [DoStmt "ret (fromIntType arg1)"]

y =
  Struct (DefiniteType "MyStruct")
  [(True,TyVar "a1")]
  [Field True (TyArgVar $ TyVar "a1") (DefiniteType "FieldA"),
   Field True (TyArgSymb $ DefTy $ DefiniteType "HLCChar") (DefiniteType "FieldB")]
  (ContSymb "cxt")
  (ContSymb "makeStructField")
  [DoStmt "fieldA <- makeStructField (Proxy :: Proxy FieldA)",
   DoStmt "fieldB <- makeStructField (Proxy :: Proxy FieldB)",
   DoStmt "return cxt"]
  (ContSymb "cxt")
  [DoStmt "return cxt"]

z = FuncCall
  (DefiniteType "SomeFunc")
  [TyVar "HLCInt"]
  [Expr "undefined"]

printFuncCall :: FuncCall -> Doc
printFuncCall (FuncCall name params args) =
  (text "_ <- call" <> (int $ length args)) <+>
  parens (text "Proxy :: Proxy" <+> parens (pp name <+> hcat (map pp params))) <+>
  hcat (map printExpr args)

printExpr :: Expr -> Doc
printExpr (Expr str) = text str

makeTypeable :: TyVar -> TyConstraint
makeTypeable = TyConstraint (DefTy $ DefiniteType "Typeable")

makeHLCTypeable :: TyVar -> TyConstraint
makeHLCTypeable = TyConstraint (DefTy $ DefiniteType "HLCTypeable")

makePassable :: TyVar -> TyConstraint
makePassable var = TyEqual (TyFamily "Passability" $ IndefTy var) (DefTy $ DefiniteType "IsPassable")

printStruct :: Struct -> Doc
printStruct (Struct {..}) =
  text "data" <+> pp structName <+> hsep (map (pp . snd) structTyParams) $+$
  text "deriving instance" <+>
  parens (hcat $ punctuate comma $ map (pp . makeTypeable . snd) structTyParams) <+>
  text "=>" <+> pp (DefTy $ DefiniteType "Typeable") <+>
  parens (pp structName <+> hsep (map (pp . snd) structTyParams)) $+$
  text "instance" <+>
  parens (hcat $ punctuate comma (map (pp . makeHLCTypeable . snd) structTyParams ++
                                  map (pp . makePassable . snd) (filter fst structTyParams))) <+>
  text "=> HLCTypeable" <+>
  parens (pp structName <+> hsep (map (pp . snd) structTyParams)) <+>
  text "where" $+$
  nest 2 (text "hlcType = structHLCType")$+$
  (vcat $ map printFieldName fields) $+$
  (vcat $ map (printStructFieldClassInst structName structTyParams) fields) $+$
  text "instance" <+>
  parens (hcat $ punctuate comma (map (pp . makeHLCTypeable . snd) structTyParams ++
                                  map (pp . makePassable . snd) (filter fst structTyParams))) <+>
  text "=> Struct" <+> text passability <+>
  parens (pp structName <+> hsep (map (pp . snd) structTyParams)) <+> text "where" $+$
  nest 2 (text "constructor _" <+> pp makeStructFieldArg <+> pp consCxtArg <+> text "= do" $+$
          nest 2 (printBody constructor) $+$
          text "destructor _" <+> pp destCxtArg <+> text "= do" $+$
          nest 2 (printBody destructor))

  
  where passability = case all fst structTyParams of
          True -> "IsPassable"
          False -> "NotPassable"

printStructFieldClassInst structName structTyParams (Field passable ty name) =
  text "instance" <+>
  parens (hcat $ punctuate comma (map (pp . makeHLCTypeable . snd) structTyParams ++
                                  map (pp . makePassable . snd) (filter fst structTyParams))) <+>
  text "=> StructFieldClass" <+> text passability <+>
  parens (pp structName <+> hsep (map (pp . snd) structTyParams)) <+>
  pp name <+> pp ty
  where passability = case passable of
          True -> "IsPassable"
          False -> "NotPassable"

printFieldName :: Field -> Doc
printFieldName (Field passable ty name) =
  text "data" <+> pp name <+> text "deriving (Typeable)"

printFunc :: Function -> Doc
printFunc (Function {..}) =
  text "data" <+> pp funcName <+> hsep (map pp funcTyParams) $+$
  text "instance" <+> parens (hcat $ punctuate comma $
                              map pp (map makeHLCTypeable funcTyParams ++
                                      tyConstraints)) <+>
  text "=> HLCFunction" <+>
  parens (pp funcName <+> hsep (map pp funcTyParams)) <+>
  parens (text "ArgWrap" <>
          (int $ length funcArgs) <+>
          (hsep $ map (parens . makeTypedExpr) funcArgTypes)) <+>
  pp retType <+>
  text "where" $+$
  nest 2 (text "call _" <+> pp retArg <+> text "=" <+>
          (text "ArgWrap" <> (int $ length funcArgs)) <+>
          parens (text "\\" <> hsep (map pp funcArgs) <+> text "-> do" $+$
                 nest 2 (printBody funcBody)))

printBody :: [DoStmt] -> Doc
printBody = vcat . map pp

makeTypedExpr :: TyArg -> Doc
makeTypedExpr arg = text "TypedExpr" <+> pp arg
  
