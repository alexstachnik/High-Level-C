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

module HighLevelC.HLCTypes where

import Language.C.Pretty(Pretty,pretty)
import Text.PrettyPrint.HughesPJ

import Data.List
import Data.Typeable

import qualified Data.Map as M
import qualified Data.Set as S

import Util.Names
import IntermediateLang.ILTypes

data Argument = Argument {argumentName :: HLCSymbol,
                          argumentType :: ILType,
                          argumentArrLen :: Maybe Integer}
              deriving (Eq,Ord,Show)

data Variable = Variable {variableName :: HLCSymbol,
                          variableType :: ILType,
                          variableArrLen :: Maybe HLCExpr,
                          variableCons :: [HLCStatement],
                          variableDest :: [HLCStatement]}
              deriving (Eq,Ord,Show)

data StructField = StructField {fieldName :: SafeName,
                                fieldType :: ILType,
                                fieldArrLen :: Maybe Integer}
                   deriving (Eq,Ord,Show)

newtype TypedStructField a = TypedStructField {fromTypedStructField :: StructField}

makeStructField :: forall st field ty.
                   (StructFieldClass st field ty) =>
                   Proxy field -> Maybe Integer -> TypedStructField st
makeStructField fname arrLen =
  TypedStructField $
  StructField (getFieldName fname) (fromTW (structHLCType :: TW st)) arrLen

newtype FuncName = FuncName {fromFuncName :: SafeName}
                     deriving (Eq,Ord,Show)

newtype StructName = StructName {fromStructName :: SafeName}
                       deriving (Eq,Ord,Show)

newtype TW a = TW {fromTW :: ILType}

class (Typeable a) => HLCTypeable a where
  hlcType :: TW a

structHLCType :: forall a. (Struct a) => TW a
structHLCType = TW $ BaseType NotConst
  (ILStructRef $ getILType (Proxy :: Proxy a))

getILType :: (Struct structType) => Proxy structType -> ILTypeName
getILType = ILTypeName . fromSafeName . fromStructName . getStructName

getObjType :: forall a. (HLCTypeable a) => a -> ILType
getObjType _ = fromTW (hlcType :: TW a)

newtype TypedExpr a = TypedExpr {fromTypedExpr :: HLCExpr}

instance (HLCTypeable a) => HLCTypeable (TypedExpr a) where
  hlcType = TW $ fromTW (hlcType :: (TW a))

newtype TypedVar a = TypedVar {fromTypedVar :: HLCSymbol} deriving (Eq,Ord,Show)

class (Typeable name, HLCTypeable retType) =>
      HLCFunction name (tyWrap :: * -> *) retType (m :: * -> *) |
  name -> tyWrap, name -> retType, name -> m where
  call :: forall r. Proxy name -> (m (TypedExpr retType) -> r) -> tyWrap r

getFuncName :: (Typeable a, HLCFunction a b c m) => Proxy a -> FuncName
getFuncName = FuncName . makeSafeName . show . typeRep

getStructName :: (Struct structType) => Proxy structType -> StructName
getStructName = StructName . makeSafeName . show . typeRep

class (HLCTypeable structType) => Struct structType where
  structContents :: Proxy structType -> [TypedStructField structType]

class (Struct structType, Typeable fieldName) =>
      StructFieldClass structType fieldName fieldType | structType fieldName -> fieldType


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

data HLCStatement = BlockStmt HLCBlock
                  | ExpStmt HLCExpr
                  | AssignmentStmt UntypedLHS HLCExpr
                  | IfThenElseStmt HLCExpr HLCBlock HLCBlock
                  deriving (Eq,Ord,Show)

data UntypedLHS = LHSVar HLCSymbol
                | LHSPtr HLCExpr
                | LHSDeref UntypedLHS
                | LHSDerefPlusOffset UntypedLHS HLCExpr
                | LHSElement UntypedLHS SafeName
                | LHSAddrOf UntypedLHS
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

instance (Typeable b, HLCTypeable a) => HLCTypeable (HLCPtr b a) where
  hlcType = TW $ PtrType NotConst $ fromTW (hlcType :: TW a)

data IsPassable
data NotPassable

type family Passability a where
  Passability (HLCPtr UniquePtr a) = NotPassable
  Passability (HLCPtr WeakPtr a) = IsPassable
  Passability a = IsPassable

type HLCWeakPtr a = HLCPtr WeakPtr a
type HLCUniquePtr a = HLCPtr UniquePtr a

data TypedLHS a where
  TypedLHSVar :: TypedVar a -> TypedLHS a
  TypedLHSPtr :: TypedExpr (HLCPtr b a) -> TypedLHS (HLCPtr b a)
  TypedLHSDeref :: TypedLHS (HLCPtr b a) -> TypedLHS a
  TypedLHSDerefPlusOffset :: (HLCBasicIntType c) =>
                             TypedLHS (HLCPtr b a) ->
                             TypedExpr c ->
                             TypedLHS a
  TypedLHSElement :: (Typeable fieldName,
                      StructFieldClass structType fieldName fieldType) =>
                     TypedLHS structType -> Proxy fieldName -> TypedLHS fieldType
  TypedLHSAddrOf :: TypedLHS a -> TypedLHS (HLCPtr WeakPtr a)

untypeLHS :: TypedLHS a -> UntypedLHS
untypeLHS (TypedLHSVar x) = LHSVar (fromTypedVar x)
untypeLHS (TypedLHSPtr x) = LHSPtr (fromTypedExpr x)
untypeLHS (TypedLHSDeref ptr) = LHSDeref (untypeLHS ptr)
untypeLHS (TypedLHSDerefPlusOffset ptr offset) =
  LHSDerefPlusOffset (untypeLHS ptr) (fromTypedExpr offset)
untypeLHS (TypedLHSElement struct field) =
  LHSElement (untypeLHS struct) (getFieldName field)
untypeLHS (TypedLHSAddrOf x) = LHSAddrOf (untypeLHS x)

lhsExpr :: TypedLHS a -> TypedExpr a
lhsExpr = TypedExpr . LHSExpr . untypeLHS

data VarArg = forall a . ConsArg (TypedExpr a) VarArg
            | NilArg

data ExtFunction a = ExtFunction HLCSymbol
                   | VarExtFunction HLCSymbol

varArgToList :: VarArg -> [HLCExpr]
varArgToList NilArg = []
varArgToList (ConsArg typedExpr rest) =
  fromTypedExpr typedExpr :
  varArgToList rest

getFieldName :: forall a. (Typeable a) => Proxy a -> SafeName
getFieldName _ = makeSafeName $ show $ typeRep (Proxy :: Proxy a)

readElt :: forall structType fieldName fieldType.
           (StructFieldClass structType fieldName fieldType, Typeable fieldName) =>
           TypedExpr structType ->
           Proxy fieldName ->
           TypedExpr fieldType
readElt struct _ =
  TypedExpr $ AccessPart (fromTypedExpr struct) $
  getFieldName (Proxy :: Proxy fieldName)

expVar :: HLCSymbol -> HLCExpr
expVar = LHSExpr . LHSVar
