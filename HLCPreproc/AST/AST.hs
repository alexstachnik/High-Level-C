module AST.AST where

import Data.Word

import Text.PrettyPrint

class Printable a where
  pp :: a -> Doc

newtype TyCon = TyCon String
              deriving (Eq,Ord,Show)
instance Printable TyCon where
  pp (TyCon str) = text str

data SomeType = DefTy TyCon
              | TyConApp SomeType [SomeType]
              | IndefTy TyVar
              | TyFamily String [SomeType]
              | NullTy
              | NatType Integer
               deriving (Eq,Ord,Show)
instance Printable SomeType where
  pp (DefTy tycon) = pp tycon
  pp (TyConApp tycon args) = parens (pp tycon <+> (hcat $ map pp args))
  pp (IndefTy var) = pp var
  pp (TyFamily family args) = parens (text family <+> (hcat $ map pp args))
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


type Name = TyCon

data Field = Field SomeType Name
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
                      isPassable :: Bool,
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

data UntypedLHS = LHSVar VarSymb
                | LHSPtr Expr
                | LHSDeref UntypedLHS
                | LHSDerefPlusOffset UntypedLHS Expr
                | LHSElement UntypedLHS SomeType
                | LHSArrAt UntypedLHS Expr
                | LHSAddrOf UntypedLHS
                deriving (Eq,Ord,Show)

data Expr = LitExpr HLCLit
          | LHSExpr UntypedLHS
          | HaskFunc VarSymb [Expr]
          | Void
          | BinOpExpr BinOp Expr Expr
          | ProxyExpr SomeType
          | ArrayAt Expr Expr
          deriving (Eq,Ord,Show)

data DoStmt = Assignment Expr Expr
            | Return Expr
            | ConsRet
            | FuncCall VarSymb Name [SomeType] [Expr]
            | MonadicBind VarSymb Expr
            | PrimVarDecl VarSymb Name
            | StructVarDecl VarSymb SomeType
            | Jump VarSymb
            | IfThenElseStmt Expr [DoStmt] [DoStmt]
            | IfThenElseRestStmt VarSymb Expr [DoStmt] [DoStmt]
            | WhileStmt Expr VarSymb VarSymb [DoStmt]
            deriving (Eq,Ord,Show)

data Export = Export Name [SomeType] Int
            deriving (Eq,Ord,Show)

data HLCDecl = FuncDecl Function
             | StructDecl Struct
             deriving (Eq,Ord,Show)

data TLU = TLU [Export] [HLCDecl]
         deriving (Eq,Ord,Show)

