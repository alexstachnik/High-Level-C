module AST.AST where

import Data.Word

import Text.PrettyPrint

class Printable a where
  pp :: a -> Doc

newtype TyCon = TyCon String
              deriving (Eq,Ord,Show)
instance Printable TyCon where
  pp (TyCon str) = text str

data SomeType = DefTy TyCon [SomeType]
              | IndefTy TyVar
              | TyFamily String SomeType
              | NullTy
              | NatType Integer
               deriving (Eq,Ord,Show)
instance Printable SomeType where
  pp (DefTy tycon []) = pp tycon
  pp (DefTy tycon args) = parens (pp tycon <+> (hcat $ map pp args))
  pp (IndefTy var) = pp var
  pp (TyFamily family arg) = parens (text family <+> pp arg)
  pp (NullTy) = text "_"
  pp (NatType n) = int $ fromIntegral n

newtype TyVar = TyVar String
                deriving (Eq,Ord,Show)
instance Printable TyVar where
  pp (TyVar str) = text str

newtype TyClass = TyClass String
             deriving (Eq,Ord,Show)
instance Printable TyClass where
  pp (TyClass str) = text str

data TyConstraint = TyConstraint TyClass SomeType
                  | TyEqual SomeType SomeType
                  deriving (Eq,Ord,Show)
instance Printable TyConstraint where
  pp (TyConstraint symb var) = pp symb <+> pp var
  pp (TyEqual lhs rhs) = pp lhs <+> text "~" <+> pp rhs

newtype VarSymb = VarSymb String
                deriving (Eq,Ord,Show)
instance Printable VarSymb where
  pp (VarSymb str) = text str


data Passability = IsPassable | NotPassable
                 deriving (Eq,Ord,Show)
instance Printable Passability where
  pp (IsPassable) = text "IsPassable"
  pp (NotPassable) = text "NotPassable"

type Name = TyCon

data Field = Field Passability SomeType Name
           deriving (Eq,Ord,Show)

retArg = VarSymb "retArg"
makeStructFieldArg = VarSymb "makeStructField"

data Function = Function {funcName :: Name,
                          funcTyParams :: [TyVar],
                          funcTyConstraints :: [TyConstraint],
                          retType :: SomeType,
                          funcArgTypes :: [SomeType],
                          funcArgs :: [VarSymb],
                          funcBody :: [DoStmt]
                         }
                deriving (Eq,Ord,Show)
                         
data Struct = Struct {structName :: Name,
                      structTyParams :: [TyVar],
                      structTyConstraints :: [TyConstraint],
                      fields :: [Field],
                      constructor :: [DoStmt],
                      destructor :: [DoStmt]}
            deriving (Eq,Ord,Show)

data HLCLit = CharLit Char
            | IntLit Integer
            | DoubleLit Double
            | StrLit String
            deriving (Eq,Ord,Show)

instance Printable HLCLit where
  pp (CharLit c) = parens (text "charLit" <+> (int $ fromEnum c))
  pp (IntLit n) = parens (text "intLit" <+> integer n)
--  pp (DoubleLit d) = double d
--  pp (StrLit str) = text (concatMap escape str)

escape :: Char -> [Char]
escape '"' = "\\\""
escape x = [x]

data BinOp = HLCPlus
           | HLCMinus
           | HLCTimes
           | HLCDivide
           | HLCRem
           | HLCLAnd
           | HLCLOr
           | HLCBitAnd
           | HLCBitOr
           | HLCBitXor
           deriving (Eq,Ord,Show)

instance Printable BinOp where
  pp HLCPlus = text "`hlcAdd`"
  pp HLCMinus = text "`hlcSub`"
  pp HLCTimes = text "`hlcMul`"
  pp HLCDivide = text "`hlcDiv`"

data UnaryOp = HLCDeref
             | HLCAddrOf
             deriving (Eq,Ord,Show)

data Expr = LitExpr HLCLit
          | Ident VarSymb
          | HaskFunc VarSymb [Expr]
          | Void
          | BinOpExpr BinOp Expr Expr
          | ProxyExpr SomeType
          | UnaryOpExpr UnaryOp Expr
          | ArrayAt Expr Expr
          deriving (Eq,Ord,Show)

data DoStmt = Assignment VarSymb Expr
            | Return Expr
            | ConsRet
            | FuncCall VarSymb Name [SomeType] [Expr]
            | VarDecl VarSymb Expr
            | PrimVarDecl VarSymb Name
            | StructVarDecl VarSymb SomeType
            | Jump VarSymb
            | IfThenElseStmt Expr [DoStmt] [DoStmt]
            | IfThenElseRestStmt VarSymb Expr [DoStmt] [DoStmt]
            | WhileStmt Expr VarSymb VarSymb [DoStmt]
            deriving (Eq,Ord,Show)

data Export = Export Name [SomeType]
            deriving (Eq,Ord,Show)

data HLCDecl = FuncDecl Function
             | StructDecl Struct
             deriving (Eq,Ord,Show)

data TLU = TLU [Export] [HLCDecl]
         deriving (Eq,Ord,Show)

