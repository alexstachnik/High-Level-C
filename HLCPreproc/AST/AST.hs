module AST.AST where

import Text.PrettyPrint

class Printable a where
  pp :: a -> Doc

newtype DefiniteType = DefiniteType String
                     deriving (Eq,Ord,Show)
instance Printable DefiniteType where
  pp (DefiniteType str) = text str

data SomeType = DefTy DefiniteType
              | IndefTy TyVar
              | TyFamily String SomeType
               deriving (Eq,Ord,Show)
instance Printable SomeType where
  pp (DefTy ty) = pp ty
  pp (IndefTy var) = pp var
  pp (TyFamily family arg) = text family <+> pp arg

newtype TyVar = TyVar String
                deriving (Eq,Ord,Show)
instance Printable TyVar where
  pp (TyVar str) = text str

data TyConstraint = TyConstraint SomeType TyVar
                  | TyEqual SomeType SomeType
                  deriving (Eq,Ord,Show)
instance Printable TyConstraint where
  pp (TyConstraint symb var) = pp symb <+> pp var
  pp (TyEqual lhs rhs) = pp lhs <+> text "~" <+> pp rhs

data TyArg = TyArgVar TyVar
           | TyArgSymb SomeType
           deriving (Eq,Ord,Show)
instance Printable TyArg where
  pp (TyArgVar var) = pp var
  pp (TyArgSymb symb) = pp symb

newtype VarSymb = VarSymb String
                deriving (Eq,Ord,Show)
instance Printable VarSymb where
  pp (VarSymb str) = text str

newtype ContSymb = ContSymb String
                 deriving (Eq,Ord,Show)
instance Printable ContSymb where
  pp (ContSymb str) = text str

newtype DoStmt = DoStmt String
               deriving (Eq,Ord,Show)
instance Printable DoStmt where
  pp (DoStmt str) = text str

data Field = Field Bool TyArg DefiniteType
           deriving (Eq,Ord,Show)

data Function = Function {funcName :: DefiniteType,
                          funcTyParams :: [TyVar],
                          tyConstraints :: [TyConstraint],
                          retType :: TyArg,
                          funcArgTypes :: [TyArg],
                          funcArgs :: [VarSymb],
                          retArg :: ContSymb,
                          funcBody :: [DoStmt]
                         }
                deriving (Eq,Ord,Show)
                         
data Struct = Struct {structName :: DefiniteType,
                      structTyParams :: [(Bool,TyVar)],
                      fields :: [Field],
                      consCxtArg :: ContSymb,
                      makeStructFieldArg :: ContSymb,
                      constructor :: [DoStmt],
                      destCxtArg :: ContSymb,
                      destructor :: [DoStmt]}
            deriving (Eq,Ord,Show)

data Expr = Expr String
          deriving (Eq,Ord,Show)

data FuncCall = FuncCall {fcallName :: DefiniteType,
                          fcallTyParams :: [TyVar],
                          fcallArgs :: [Expr]}
              deriving (Eq,Ord,Show)


  

