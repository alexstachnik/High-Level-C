{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HighLevelC.HLCTypes where

import Language.C.Pretty(Pretty,pretty)
import Text.PrettyPrint.HughesPJ

import Control.Monad.State
import Control.Monad.Writer(WriterT,execWriterT,MonadWriter,tell,listen)
import Control.Monad.Identity(Identity(runIdentity))

import Data.List
import Data.Typeable
import Data.Type.Equality
import Data.Sequence((><),(|>))
import qualified Data.Sequence as Sq
import qualified Data.Map as M
import qualified Data.Set as S

import GHC.TypeLits

import Util.Names
import IntermediateLang.ILTypes

data Argument = Argument {argumentName :: HLCSymbol,
                          argumentType :: ILType,
                          argumentArrLen :: Maybe Integer}
              deriving (Eq,Ord,Show)

data Variable = Variable {variableName :: HLCSymbol,
                          variableType :: ILType,
                          variableArrLen :: Maybe HLCExpr,
                          variableCons :: HLCBlock,
                          variableDest :: HLCBlock}
              deriving (Eq,Ord,Show)

data StructField = StructField {fieldName :: SafeName,
                                fieldType :: ILType,
                                fieldArrLen :: Maybe Integer}
                   deriving (Eq,Ord,Show)

data CWriter = CWriter {functionProtos :: Sq.Seq FunctionProto,
                        structProtos :: Sq.Seq StructProto,
                        structDefs :: Sq.Seq StructDef,
                        funcDefs :: Sq.Seq FunctionDef,
                        stmts :: Sq.Seq HLCStatement,
                        varDecls :: Sq.Seq Variable,
                        structVarDecls :: Sq.Seq StructField}
             deriving (Show)

data CodeState = CodeState {funcInstances :: M.Map FuncName HLCSymbol,
                            structInstances :: M.Map StructName HLCSymbol,
                            uidState :: Integer}
                 deriving (Show)


newtype HLC_ a = HLC_ {
  runHLC_ :: (WriterT CWriter
              (StateT CodeState Identity) a)}
              deriving (Monad,
                        Applicative,
                        Functor,
                        MonadState CodeState,
                        MonadWriter CWriter)

newtype HLC a = HLC {innerHLC :: HLC_ a}
              deriving (Monad,Applicative,Functor)



instance Monoid CWriter where
  mempty = CWriter {functionProtos = Sq.empty,
                    structProtos = Sq.empty,
                    structDefs = Sq.empty,
                    funcDefs = Sq.empty,
                    stmts = Sq.empty,
                    varDecls = Sq.empty,
                    structVarDecls = Sq.empty}
  mappend a b = CWriter {functionProtos = functionProtos a >< functionProtos b,
                         structProtos = structProtos a >< structProtos b,
                         structDefs = structDefs a >< structDefs b,
                         funcDefs = funcDefs a >< funcDefs b,
                         stmts = stmts a >< stmts b,
                         varDecls = varDecls a >< varDecls b,
                         structVarDecls = Sq.empty}


newtype FuncName = FuncName {fromFuncName :: SafeName}
                     deriving (Eq,Ord,Show)

newtype StructName = StructName {fromStructName :: SafeName}
                       deriving (Eq,Ord,Show)

newtype TW a = TW {fromTW :: ILType}

class (Typeable a) => HLCTypeable a where
  hlcType :: TW a

newtype TypedExpr a = TypedExpr {fromTypedExpr :: HLCExpr}

instance (HLCTypeable a) => HLCTypeable (TypedExpr a) where
  hlcType = TW $ fromTW (hlcType :: (TW a))

newtype TypedVar a = TypedVar {fromTypedVar :: HLCSymbol} deriving (Eq,Ord,Show)

class (Typeable name, HLCTypeable retType) =>
      HLCFunction name (tyWrap :: * -> *) retType |
  name -> tyWrap, name -> retType where
  call :: forall r. Proxy name -> (TypedExpr retType -> r) -> tyWrap r

type family PermissibleStruct struct field where
  PermissibleStruct IsPassable IsPassable = True
  PermissibleStruct NotPassable IsPassable = True
  PermissibleStruct NotPassable NotPassable = True
  PermissibleStruct IsPassable NotPassable = False

class (HLCTypeable structType) => Struct passable structType | structType -> passable where
  constructor :: Proxy structType ->
                 (forall fieldName arrLen fieldType.
                  (KnownNat arrLen,
                   StructFieldClass passable structType fieldName fieldType arrLen) =>
                  Proxy fieldName -> HLC (TypedVar fieldType)) ->
                 HLC ()
  destructor :: Proxy structType -> HLC ()

type family OkArr arrLenGtOne fieldType where
  OkArr True (HLCArray a) = True
  OkArr True b = False
  OkArr False b = True

class (Struct passable structType,
       Typeable fieldName,
       PermissibleStruct passable (Passability fieldType) ~ True,
       HLCTypeable fieldType,
       OkArr (2 <=? arrLen) fieldType ~ True) =>
      StructFieldClass passable structType fieldName fieldType (arrLen :: Nat) |
  structType fieldName -> fieldType,
  structType fieldName -> arrLen

data FunctionProto = FunctionProto ILType HLCSymbol [Argument]
                      deriving (Show,Eq,Ord)

data StructProto = StructProto HLCSymbol
                 deriving (Show,Eq,Ord)

data FunctionDef = FunctionDef {fdefRetType :: ILType,
                                fdefName :: HLCSymbol,
                                fdefArguments :: [Argument],
                                fdefBody :: HLCBlock}
                 deriving (Show,Eq,Ord)

data StructDef = StructDef HLCSymbol [StructField]
               deriving (Eq,Ord,Show)

data HLCBlock = HLCBlock {blockVars :: [Variable],
                          blockStmts :: [HLCStatement]}
              deriving (Eq,Ord,Show)

emptyBlock = HLCBlock [] []

data HLCStatement = BlockStmt HLCBlock
                  | ExpStmt HLCExpr
                  | AssignmentStmt UntypedLHS HLCExpr
                  | IfThenElseStmt HLCExpr HLCBlock HLCBlock
                  | WhileStmt HLCExpr HLCBlock
                  | GotoStmt HLCSymbol
                  | ReturnStmt HLCExpr
                  deriving (Eq,Ord,Show)

data UntypedLHS = LHSVar HLCSymbol
                | LHSPtr HLCExpr
                | LHSDeref UntypedLHS
                | LHSDerefPlusOffset UntypedLHS HLCExpr
                | LHSElement UntypedLHS SafeName
                | LHSAddrOf UntypedLHS
                | LHSArrAt UntypedLHS HLCExpr
                deriving (Eq,Ord,Show)

data HLCExpr = LHSExpr UntypedLHS
             | FunctionCall HLCExpr [HLCExpr]
             | LitExpr HLCLit
             | AccessPart HLCExpr SafeName
             | SizeOf ILType
             | ExprBinOp HLCBinOp HLCExpr HLCExpr
             | HLCCast ILType HLCExpr
             | Void
             deriving (Eq,Ord,Show)

data HLCBinOp = HLCPlus
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

data HLCLit = CharLit Char
            | IntLit Integer
            | DoubleLit Double
            | StrLit String
            deriving (Eq,Ord,Show)

class (HLCTypeable a) => HLCBasicIntType a
class (HLCTypeable a) => HLCPrimType a

instance (Typeable b, HLCPrimType a) => HLCPrimType (HLCPtr b a)

newtype HLCPtr ptrType a = HLCPtr a deriving (Typeable)

data WeakPtr deriving (Typeable)
data UniquePtr deriving (Typeable)
data SmartPtr deriving (Typeable)

instance (Typeable b, HLCTypeable a) => HLCTypeable (HLCPtr b a) where
  hlcType = TW $ PtrType NotConst $ fromTW (hlcType :: TW a)

data IsPassable
data NotPassable

type family Passability a where
  Passability (HLCPtr UniquePtr a) = NotPassable
  Passability (HLCPtr SmartPtr a) = NotPassable
  Passability (HLCPtr WeakPtr a) = IsPassable
  Passability a = IsPassable

type HLCWeakPtr a = HLCPtr WeakPtr a
type HLCSmartPtr a = HLCPtr SmartPtr a
type HLCUniquePtr a = HLCPtr UniquePtr a

newtype HLCArray a = HLCArray a deriving (Typeable)

instance HLCTypeable a => HLCTypeable (HLCArray a) where
  hlcType = TW $ ArrType $ fromTW (hlcType :: TW a)

data TypedLHS a where
  TypedLHSVar :: TypedVar a -> TypedLHS a
  TypedLHSPtr :: TypedExpr (HLCPtr b a) -> TypedLHS (HLCPtr b a)
  TypedLHSDeref :: TypedLHS (HLCPtr b a) -> TypedLHS a
  TypedLHSDerefPlusOffset :: (HLCBasicIntType c) =>
                             TypedLHS (HLCPtr b a) ->
                             TypedExpr c ->
                             TypedLHS a
  TypedLHSElement :: (Typeable fieldName,
                      StructFieldClass p structType fieldName fieldType arrLen) =>
                     TypedLHS structType -> Proxy fieldName -> TypedLHS fieldType
  TypedLHSArrAt :: (HLCBasicIntType c) =>
                   TypedLHS (HLCArray a) ->
                   TypedExpr c ->
                   TypedLHS a
  TypedLHSAddrOf :: TypedLHS a -> TypedLHS (HLCPtr WeakPtr a)

data VarArg = forall a . ConsArg (TypedExpr a) VarArg
            | NilArg

data ExtFunction a = ExtFunction HLCSymbol
                   | VarExtFunction HLCSymbol

untypeLHS :: TypedLHS a -> UntypedLHS
untypeLHS (TypedLHSVar x) = LHSVar (fromTypedVar x)
untypeLHS (TypedLHSPtr x) = LHSPtr (fromTypedExpr x)
untypeLHS (TypedLHSDeref ptr) = LHSDeref (untypeLHS ptr)
untypeLHS (TypedLHSDerefPlusOffset ptr offset) =
  LHSDerefPlusOffset (untypeLHS ptr) (fromTypedExpr offset)
untypeLHS (TypedLHSElement struct field) =
  LHSElement (untypeLHS struct) (getFieldName field)
untypeLHS (TypedLHSArrAt arr ix) =
  LHSArrAt (untypeLHS arr) (fromTypedExpr ix)
untypeLHS (TypedLHSAddrOf x) = LHSAddrOf (untypeLHS x)


lhsExpr :: TypedLHS a -> TypedExpr a
lhsExpr = TypedExpr . LHSExpr . untypeLHS

varArgToList :: VarArg -> [HLCExpr]
varArgToList NilArg = []
varArgToList (ConsArg typedExpr rest) =
  fromTypedExpr typedExpr :
  varArgToList rest

getFieldName :: forall a. (Typeable a) => Proxy a -> SafeName
getFieldName _ = makeSafeName $ show $ typeRep (Proxy :: Proxy a)

readElt :: forall structType fieldName fieldType p arrLen.
           (StructFieldClass p structType fieldName fieldType arrLen, Typeable fieldName) =>
           TypedExpr structType ->
           Proxy fieldName ->
           TypedExpr fieldType
readElt struct _ =
  TypedExpr $ AccessPart (fromTypedExpr struct) $
  getFieldName (Proxy :: Proxy fieldName)

expVar :: HLCSymbol -> HLCExpr
expVar = LHSExpr . LHSVar


getFuncName :: (Typeable a, HLCFunction a b c) => Proxy a -> FuncName
getFuncName = FuncName . makeSafeName . show . typeRep

getStructName :: (Struct p structType) => Proxy structType -> StructName
getStructName = StructName . makeSafeName . show . typeRep

getILType :: (Struct p structType) => Proxy structType -> ILTypeName
getILType = ILTypeName . fromSafeName . fromStructName . getStructName

getObjType :: forall a. (HLCTypeable a) => a -> ILType
getObjType _ = fromTW (hlcType :: TW a)



structHLCType :: forall a p. (Struct p a) => TW a
structHLCType = TW $ BaseType NotConst
  (ILStructRef $ getILType (Proxy :: Proxy a))

