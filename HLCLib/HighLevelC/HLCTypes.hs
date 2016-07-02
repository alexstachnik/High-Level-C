
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}


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

import Data.Word
import Data.List
import Data.Typeable
import Data.Data
import Data.Type.Equality
import Data.Sequence((><),(|>))
import qualified Data.Sequence as Sq
import qualified Data.Map as M
import qualified Data.Set as S

import GHC.TypeLits

import Util.Names
import IntermediateLang.ILTypes

data Argument = Argument {argumentName :: HLCSymbol,
                          argumentType :: ILType}
              deriving (Eq,Ord,Show,Data,Typeable)

data Variable = Variable {variableName :: HLCSymbol,
                          variableType :: ILType,
                          variableArrLen :: Maybe HLCExpr,
                          variableCons :: HLCBlock,
                          variableDest :: HLCBlock}
              deriving (Eq,Ord,Show,Data,Typeable)

data StructField = StructField {fieldName :: SafeName,
                                fieldType :: ILType}
                   deriving (Eq,Ord,Show,Data,Typeable)

newtype PreprocessorDirective = PreprocessorDirective String
                              deriving (Eq,Ord,Show,Data,Typeable)

data CWriter = CWriter {functionProtos :: Sq.Seq FunctionProto,
                        structProtos :: Sq.Seq StructProto,
                        structDefs :: Sq.Seq StructDef,
                        funcDefs :: Sq.Seq FunctionDef,
                        stmts :: Sq.Seq HLCStatement,
                        varDecls :: Sq.Seq Variable,
                        structVarDecls :: Sq.Seq StructField,
                        preproDirs :: S.Set PreprocessorDirective}
             deriving (Eq,Ord,Show,Data,Typeable)

data CodeState = CodeState {funcInstances :: M.Map FuncName (HLCSymbol,Variable),
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
                    structVarDecls = Sq.empty,
                    preproDirs = S.empty}
  mappend a b = CWriter {functionProtos = functionProtos a >< functionProtos b,
                         structProtos = structProtos a >< structProtos b,
                         structDefs = structDefs a >< structDefs b,
                         funcDefs = funcDefs a >< funcDefs b,
                         stmts = stmts a >< stmts b,
                         varDecls = varDecls a >< varDecls b,
                         structVarDecls = structVarDecls a >< structVarDecls b,
                         preproDirs = mappend (preproDirs a) (preproDirs b)}


newtype FuncName = FuncName {fromFuncName :: SafeName}
                     deriving (Eq,Ord,Show,Data,Typeable)

newtype StructName = StructName {fromStructName :: SafeName}
                       deriving (Eq,Ord,Show,Data,Typeable)

newtype TW a = TW {fromTW :: ILType}

class (Typeable a) => HLCTypeable a where
  hlcType :: TW a

newtype TypedExpr a = TypedExpr {fromTypedExpr :: HLCExpr}

instance (HLCTypeable a) => HLCTypeable (TypedExpr a) where
  hlcType = TW $ fromTW (hlcType :: (TW a))

newtype TypedVar a = TypedVar {fromTypedVar :: HLCSymbol} deriving (Eq,Ord,Show,Data,Typeable)

class (Typeable name, HLCTypeable retType) =>
      HLCFunction name (tyWrap :: * -> *) retType |
  name -> tyWrap, name -> retType where
  call :: Proxy name -> (HLC (TypedExpr retType) -> HLC Context) -> tyWrap (HLC Context)

type family PermissibleStruct struct field where
  PermissibleStruct IsPassable IsPassable = True
  PermissibleStruct NotPassable IsPassable = True
  PermissibleStruct NotPassable NotPassable = True
  PermissibleStruct IsPassable NotPassable = False

class (HLCTypeable structType) => Struct passable structType | structType -> passable where
  constructor :: Proxy structType ->
                 HLC (TypedLHS structType) ->
                 Context ->
                 HLC Context
  destructor :: Proxy structType -> Context -> HLC Context
  fieldList :: Proxy structType -> [StructField]

class (Struct passable structType,
       Typeable fieldName,
       PermissibleStruct passable (Passability fieldType) ~ True,
       HLCTypeable fieldType) =>
      StructFieldClass passable structType fieldName fieldType |
  structType fieldName -> fieldType

class (HLCTypeable a) => Instanciable a (b :: Bool) where
  declareObj' :: Proxy '(a,b) -> HLC ()
  construct' :: Proxy '(a,b) -> HLC (TypedLHS a) -> Context -> HLC Context
  destruct' :: Proxy '(a,b) -> Context -> HLC Context

instance (HLCTypeable a) => Instanciable a True where
  declareObj' _ = return ()
  construct' _ _ = return
  destruct' _ = return

declareObj :: forall a. (Instanciable a (IsPrimitive a)) =>
              Proxy a -> HLC ()
declareObj _ = declareObj' (Proxy :: Proxy '(a,IsPrimitive a))

construct :: forall a. (Instanciable a (IsPrimitive a)) =>
             Proxy a -> HLC (TypedLHS a) -> Context -> HLC Context
construct _ = construct' (Proxy :: Proxy '(a,IsPrimitive a))

destruct :: forall a. (Instanciable a (IsPrimitive a)) =>
            Proxy a -> Context -> HLC Context
destruct _ = destruct' (Proxy :: Proxy '(a,IsPrimitive a))

type family IsPrimitive a :: Bool where
  IsPrimitive HLCVoid = True
  IsPrimitive HLCInt = True
  IsPrimitive HLCChar = True
  IsPrimitive HLCDouble = True
  IsPrimitive HLCString = True
  IsPrimitive HLCInt8 = True
  IsPrimitive HLCInt16 = True
  IsPrimitive HLCInt32 = True
  IsPrimitive HLCInt64 = True
  IsPrimitive HLCUInt8 = True
  IsPrimitive HLCUInt16 = True
  IsPrimitive HLCUInt32 = True
  IsPrimitive HLCUInt64 = True
  IsPrimitive HLCBool = True
  IsPrimitive a = False

data FunctionProto = FunctionProto ILType HLCSymbol [Argument]
                      deriving (Show,Eq,Ord,Data,Typeable)

data StructProto = StructProto HLCSymbol
                 deriving (Show,Eq,Ord,Data,Typeable)

data FunctionDef = FunctionDef {fdefRetType :: ILType,
                                fdefName :: HLCSymbol,
                                fdefArguments :: [Argument],
                                fdefBody :: HLCBlock,
                                fdefRetVar :: Variable}
                 deriving (Show,Eq,Ord,Data,Typeable)

data StructDef = StructDef HLCSymbol [StructField]
               deriving (Eq,Ord,Show,Data,Typeable)

newtype StatementList = StatementList {fromStatementList :: [HLCStatement]}
                      deriving (Eq,Ord,Show,Data,Typeable)

data HLCBlock = HLCBlock {blockVars :: [Variable],
                          blockStmts :: StatementList,
                          blockRetCxt :: Context}
              deriving (Eq,Ord,Show,Data,Typeable)

data Context = NullContext Variable HLCExpr
             | VoidReturn
             | SomeContext HLCSymbol
             | NextLine
             deriving (Eq,Ord,Show,Data,Typeable)

data HLCStatement = BlockStmt HLCBlock
                  | ExpStmt HLCExpr
                  | LabelStmt HLCSymbol
                  | JumpStmt HLCSymbol
                  | AssignmentStmt UntypedLHS HLCExpr
                  | IfThenElseRestStmt HLCExpr HLCSymbol HLCBlock HLCBlock
                  | IfThenElseStmt HLCExpr HLCBlock HLCBlock
                  | WhileStmt HLCExpr HLCSymbol HLCSymbol HLCBlock
                  deriving (Eq,Ord,Show,Data,Typeable)

data UntypedLHS = LHSVar HLCSymbol
                | LHSPtr HLCExpr
                | LHSDeref UntypedLHS
                | LHSDerefPlusOffset UntypedLHS HLCExpr
                | LHSElement UntypedLHS SafeName
                | LHSAddrOf UntypedLHS
                deriving (Eq,Ord,Show,Data,Typeable)

data HLCExpr = LHSExpr UntypedLHS
             | FunctionCall HLCExpr [HLCExpr]
             | LitExpr HLCLit
             | AccessPart HLCExpr SafeName
             | SizeOf ILType
             | ExprBinOp HLCBinOp HLCExpr HLCExpr
             | ExprNegate HLCExpr
             | HLCCast ILType HLCExpr
             | HLCTernary HLCExpr HLCExpr HLCExpr
             | Void
             deriving (Eq,Ord,Show,Data,Typeable)

data HLCBinOp = HLCPlus
              | HLCMinus
              | HLCTimes
              | HLCDivide
              | HLCEqual
              | HLCLT
              | HLCGT
              | HLCLTEQ
              | HLCGTEQ
              | HLCSHL
              | HLCSHR
              | HLCRem
              | HLCLAnd
              | HLCLOr
              | HLCBitAnd
              | HLCBitOr
              | HLCBitXor
              deriving (Eq,Ord,Show,Data,Typeable)

data HLCLit = CharLit Word8
            | IntLit Integer
            | DoubleLit Double
            | StrLit String
            deriving (Eq,Ord,Show,Data,Typeable)

class (HLCTypeable a) => HLCBasicIntType a
class (HLCTypeable a) => HLCNumType a where
  hlcFromInteger :: Integer -> HLC (TypedExpr a)


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

type family MkWeakPtr a where
  MkWeakPtr (HLC (TypedExpr a)) = HLC (TypedExpr (HLCWeakPtr a))

newtype HLCArray a (arrLen :: Nat) = HLCArray a deriving (Typeable)

instance (HLCTypeable a, KnownNat arrLen, Typeable arrLen) => HLCTypeable (HLCArray a arrLen) where
  hlcType = case natVal (Proxy :: Proxy arrLen) of
    0 -> TW $ ArrType (fromTW (hlcType :: TW a)) Nothing
    n -> TW $ ArrType (fromTW (hlcType :: TW a)) (Just n)

data TypedLHS a where
  TypedLHSVar :: TypedVar a -> TypedLHS a
  TypedLHSPtr :: TypedExpr (HLCPtr b a) -> TypedLHS (HLCPtr b a)
  TypedLHSDeref :: TypedLHS (HLCPtr b a) -> TypedLHS a
  TypedLHSDerefPlusOffset :: (HLCBasicIntType c) =>
                             TypedLHS (HLCPtr b a) ->
                             TypedExpr c ->
                             TypedLHS a
  TypedLHSElement :: (Typeable fieldName,
                      StructFieldClass p structType fieldName fieldType) =>
                     TypedLHS structType -> Proxy fieldName -> TypedLHS fieldType
  TypedLHSArrAt :: (HLCBasicIntType c) =>
                   TypedLHS (HLCArray a arrLen) ->
                   TypedExpr c ->
                   TypedLHS a
  TypedLHSAddrOf :: TypedLHS a -> TypedLHS (HLCPtr WeakPtr a)

data VarArg = forall a . ConsArg (TypedExpr a) VarArg
            | NilArg

data ExtFunction a = ExtFunction HLCSymbol [PreprocessorDirective]
                   | VarExtFunction HLCSymbol [PreprocessorDirective]

extFunctionName :: ExtFunction a -> HLCSymbol
extFunctionName (ExtFunction name _) = name
extFunctionName (VarExtFunction name _) = name

extFunctionDirs :: ExtFunction a -> [PreprocessorDirective]
extFunctionDirs (ExtFunction _ dirs) = dirs
extFunctionDirs (VarExtFunction _ dirs) = dirs

class (HLCTypeable b) => RHSExpression a b | a -> b where
  rhsExpr :: a -> HLC (TypedExpr b)

instance (HLCTypeable a) => RHSExpression (HLC (TypedExpr a)) a where
  rhsExpr = id

instance (HLCTypeable a) => RHSExpression (TypedLHS a) a where
  rhsExpr = lhsExpr

class (HLCTypeable b) => LHSExpression a b | a -> b where
  hlcLHSExpr :: a -> HLC (TypedLHS b)

instance (HLCTypeable b) => LHSExpression (HLC (TypedLHS b)) b where
  hlcLHSExpr = id

instance (HLCTypeable a) => LHSExpression (TypedLHS a) a where
  hlcLHSExpr = return

data HLCVoid
instance HLCTypeable HLCVoid where
  hlcType = TW (BaseType NotConst ILVoid)

data HLCInt
data HLCChar
data HLCDouble
data HLCString
data HLCInt8
data HLCInt16
data HLCInt32
data HLCInt64
data HLCUInt8
data HLCUInt16
data HLCUInt32
data HLCUInt64
data HLCBool



untypeLHS :: TypedLHS a -> UntypedLHS
untypeLHS (TypedLHSVar x) = LHSVar (fromTypedVar x)
untypeLHS (TypedLHSPtr x) = LHSPtr (fromTypedExpr x)
untypeLHS (TypedLHSDeref ptr) = LHSDeref (untypeLHS ptr)
untypeLHS (TypedLHSDerefPlusOffset ptr offset) =
  LHSDerefPlusOffset (untypeLHS ptr) (fromTypedExpr offset)
untypeLHS (TypedLHSElement struct field) =
  LHSElement (untypeLHS struct) (getFieldName field)
untypeLHS (TypedLHSArrAt arr ix) =
  LHSDerefPlusOffset (untypeLHS arr) (fromTypedExpr ix)
untypeLHS (TypedLHSAddrOf x) = LHSAddrOf (untypeLHS x)

lhsExpr :: TypedLHS a -> HLC (TypedExpr a)
lhsExpr = return . TypedExpr . LHSExpr . untypeLHS

lhsVar :: TypedLHS (TypedVar a) -> HLC (TypedExpr a)
lhsVar = return . TypedExpr . LHSExpr . untypeLHS

varArgToList :: VarArg -> [HLCExpr]
varArgToList NilArg = []
varArgToList (ConsArg typedExpr rest) =
  fromTypedExpr typedExpr :
  varArgToList rest

getFieldName :: forall a. (Typeable a) => Proxy a -> SafeName
getFieldName _ = makeSafeName $ show $ typeRep (Proxy :: Proxy a)

readElt :: forall structType fieldName fieldType p.
           (StructFieldClass p structType fieldName fieldType, Typeable fieldName) =>
           HLC (TypedExpr structType) ->
           Proxy fieldName ->
           HLC (TypedExpr fieldType)
readElt struct _ = do
  struct' <- struct
  return $ TypedExpr $ AccessPart (fromTypedExpr struct') $
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

emptyBlock = HLCBlock [] (StatementList []) NextLine

addVarToBlock :: Variable -> HLCBlock -> HLCBlock
addVarToBlock var (HLCBlock {..}) = HLCBlock {blockVars=var:blockVars,..}


