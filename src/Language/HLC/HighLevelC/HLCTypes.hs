{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}

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

module Language.HLC.HighLevelC.HLCTypes where

import Language.C.Pretty(Pretty,pretty)
import Text.PrettyPrint.HughesPJ

import Control.Monad.State
import Control.Monad.Writer(WriterT,execWriterT,MonadWriter,tell,listen)
import Control.Monad.Identity(Identity(runIdentity))

import Type.Bool
import Type.List
import Type.Container
import Type.Map

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
import GHC.Exts

import Language.HLC.Util.Names
import Language.HLC.IntermediateLang.ILTypes

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

type family CreateFunVariableArgs (argList :: [*]) :: * where
  CreateFunVariableArgs (x ': xs) = (->) (HLC (TypedLHS x)) (CreateFunVariableArgs xs)
  CreateFunVariableArgs '[] = HLC Context

type family ArgCapture (argList :: [*]) :: * where
  ArgCapture (x ': xs) = (->) (HLC (TypedExpr x)) (ArgCapture xs)
  ArgCapture '[] = [HLC HLCExpr]

type family OutwardFacingFun (argList :: [*]) retType where
  OutwardFacingFun (x ': xs) retType = (->) (HLC (TypedExpr x)) (OutwardFacingFun xs retType)
  OutwardFacingFun '[] retType = HLC (TypedExpr retType)

type FuncType retType args = (forall a. (RHSExpression a retType) => a -> HLC Context) -> CreateFunVariableArgs args

class (Typeable funName,
       HLCTypeable retType) =>
      HLCFunction (funName :: *) (argList :: [*]) (retType :: *) |
  funName -> argList,
  funName -> retType where
  thisFun :: Proxy funName ->
             (forall a. (RHSExpression a retType) => a -> HLC Context) ->
             CreateFunVariableArgs argList

class (HLCFunction funName argList retType) => GetFunc funName (argList :: [*]) retType where
  getFunc :: Proxy funName -> OutwardFacingFun argList retType

class GetArgs (argList :: [*]) retType where
  getArgs :: Proxy argList ->
             Proxy retType ->
             [HLC (Argument,HLCExpr)] ->
             ([HLC (Argument,HLCExpr)] -> HLC (TypedExpr retType)) ->
             OutwardFacingFun argList retType

instance GetArgs '[] retType where
  getArgs _ _ argList f = f argList

class ApplyDummyVars func where
  applyDummyVars :: func -> [HLCSymbol] -> HLC Context

instance (ApplyDummyVars b) => ApplyDummyVars (HLC (TypedLHS a) -> b) where
  applyDummyVars f (symb:rest) = applyDummyVars (f (return $ TypedLHSVar $ TypedVar symb)) rest

instance ApplyDummyVars (HLC Context) where
  applyDummyVars f [] = f

type family PermissibleStruct struct field where
  PermissibleStruct IsPassable IsPassable = True
  PermissibleStruct NotPassable IsPassable = True
  PermissibleStruct NotPassable NotPassable = True
  PermissibleStruct IsPassable NotPassable = False

class (HLCTypeable structType) => Struct structType where
  type StructPassability structType
  type StructFields structType :: [(*,*)]
  constructor :: Proxy structType ->
                 HLC (TypedLHS structType) ->
                 HLC Context ->
                 HLC Context
  destructor :: Proxy structType ->
                HLC (TypedLHS structType) ->
                HLC Context ->
                HLC Context

class (Struct structType,
       Typeable fieldName,
       InFstElts structType fieldName,
       GetFieldType structType fieldName ~ fieldType,
       PermissibleStruct (StructPassability structType) (Passability fieldType) ~ True,
       HLCTypeable fieldType) =>
      StructFieldClass structType fieldName fieldType |
  structType fieldName -> fieldType

instance (Struct structType,
          Typeable fieldName,
          InFstElts structType fieldName,
          PermissibleStruct (StructPassability structType) (Passability fieldType) ~ True,
          GetFieldType structType fieldName ~ fieldType,
          HLCTypeable fieldType) => StructFieldClass structType fieldName fieldType

class (HLCTypeable a) => Instanciable a (b :: Bool) where
  declareObj' :: Proxy '(a,b) -> HLC ()
  construct' :: Proxy '(a,b) -> HLC (TypedLHS a) -> HLC Context -> HLC Context
  destruct' :: Proxy '(a,b) -> HLC (TypedLHS a) -> HLC Context -> HLC Context

instance (HLCTypeable a) => Instanciable a True where
  declareObj' _ = return ()
  construct' _ _ = id
  destruct' _ _ = id

declareObj :: forall a. (Instanciable a (IsPrimitive a)) =>
              Proxy a -> HLC ()
declareObj _ = declareObj' (Proxy :: Proxy '(a,IsPrimitive a))

construct :: forall a. (Instanciable a (IsPrimitive a)) =>
             Proxy a -> HLC (TypedLHS a) -> HLC Context -> HLC Context
construct _ = construct' (Proxy :: Proxy '(a,IsPrimitive a))

destruct :: forall a. (Instanciable a (IsPrimitive a)) =>
            Proxy a -> HLC (TypedLHS a) -> HLC Context -> HLC Context
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
  IsPrimitive (FunctionPtr a b) = True
  IsPrimitive (HLCPtr a) = True
  IsPrimitive (HLCPrimArray a b) = True
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


newtype HLCPtr a = HLCPtr a deriving (Typeable)

instance (HLCTypeable a) => HLCTypeable (HLCPtr a) where
  hlcType = TW $ PtrType NotConst $ fromTW (hlcType :: TW a)

data IsPassable
data NotPassable

type Passability a = If (IsPrimitive a) IsPassable (StructPassability a)

type family MkPtr a where
  MkPtr (HLC (TypedExpr a)) = HLC (TypedExpr (HLCPtr a))
  MkPtr (HLC (TypedLHS a)) = HLC (TypedLHS (HLCPtr a))

newtype HLCPrimArray a (arrLen :: Nat) = HLCPrimArray a deriving (Typeable)

instance (IsPrimitive a ~ True, HLCTypeable a,
          KnownNat arrLen, Typeable arrLen) => HLCTypeable (HLCPrimArray a arrLen) where
  hlcType = case natVal (Proxy :: Proxy arrLen) of
    0 -> TW $ ArrType (fromTW (hlcType :: TW a)) Nothing
    n -> TW $ ArrType (fromTW (hlcType :: TW a)) (Just n)

data TypedLHS a where
  TypedLHSVar :: TypedVar a -> TypedLHS a
  TypedLHSPtr :: TypedExpr (HLCPtr a) -> TypedLHS (HLCPtr a)
  TypedLHSDeref :: TypedLHS (HLCPtr a) -> TypedLHS a
  TypedLHSDerefPlusOffset :: (HLCBasicIntType c) =>
                             TypedLHS (HLCPtr a) ->
                             TypedExpr c ->
                             TypedLHS a
  TypedLHSElement :: (Typeable fieldName,
                      StructFieldClass structType fieldName fieldType) =>
                     TypedLHS structType -> Proxy fieldName -> TypedLHS fieldType
  TypedLHSArrAt :: (HLCBasicIntType c) =>
                   TypedLHS (HLCPrimArray a arrLen) ->
                   TypedExpr c ->
                   TypedLHS a
  TypedLHSAddrOf :: TypedLHS a -> TypedLHS (HLCPtr a)


data ExtFunction (args :: [*]) retType (isVariadic :: Bool) = ExtFunction HLCSymbol [PreprocessorDirective]

data FunctionPtr (args :: [*]) retType = FunctionPtr deriving (Typeable)

class TypeableList (tys :: [*]) where
  getTys :: Proxy tys -> [ILType]
instance (TypeableList xs, HLCTypeable x) => TypeableList (x ': xs) where
  getTys _ = fromTW (hlcType :: TW x) : getTys (Proxy :: Proxy xs)
instance TypeableList '[] where
  getTys _ = []
  
instance (TypeableList args,
          HLCTypeable retType,
          Typeable (FunctionPtr args retType)) =>
         HLCTypeable (FunctionPtr args retType) where
  hlcType =
    let ilRetType = fromTW (hlcType :: TW retType)
        ilArgs = getTys (Proxy :: Proxy args)
    in
    TW (PtrType NotConst (FuncType ilRetType ilArgs))

addrOfExtFunc :: ExtFunction args retType False -> HLC (TypedExpr (FunctionPtr args retType))
addrOfExtFunc (ExtFunction symb _) = return $ TypedExpr $ LHSExpr $ LHSAddrOf $ LHSVar symb

addrOfFunc :: (HLCFunction fname argList retType) =>
              Proxy fname -> HLC (TypedExpr (FunctionPtr argList retType))
addrOfFunc proxyName =
  return $ TypedExpr $ LHSExpr $ LHSAddrOf $
  LHSVar $ ExactSymbol $ fromSafeName $ fromFuncName $ getFuncName proxyName

class (HLCTypeable b) => RHSExpression a b | a -> b where
  rhsExpr :: a -> HLC (TypedExpr b)

instance (HLCTypeable a) => RHSExpression (HLC (TypedExpr a)) a where
  rhsExpr = id

instance (HLCTypeable a) => RHSExpression (HLC (TypedLHS a)) a where
  rhsExpr l = l >>= lhsExpr

instance (HLCTypeable a) => RHSExpression (TypedLHS a) a where
  rhsExpr = lhsExpr

class (HLCTypeable b) => LHSExpression a b | a -> b where
  hlcLHSExpr :: a -> HLC (TypedLHS b)

instance (HLCTypeable b) => LHSExpression (HLC (TypedLHS b)) b where
  hlcLHSExpr = id

instance (HLCTypeable a) => LHSExpression (TypedLHS a) a where
  hlcLHSExpr = return

data HLCVoid = HLCVoid
instance HLCTypeable HLCVoid where
  hlcType = TW (BaseType NotConst ILVoid)

data HLCInt = HLCInt
data HLCChar = HLCChar
data HLCDouble = HLCDouble
data HLCString = HLCString
data HLCInt8 = HLCInt8
data HLCInt16 = HLCInt16
data HLCInt32 = HLCInt32
data HLCInt64 = HLCInt64
data HLCUInt8 = HLCUInt8
data HLCUInt16 = HLCUInt16
data HLCUInt32 = HLCUInt32
data HLCUInt64 = HLCUInt64
data HLCBool = HLCBool

  

class GetStructFields (fieldPairs :: [(*,*)]) where
  getStructFields :: Proxy fieldPairs -> [StructField]
instance GetStructFields '[] where
  getStructFields _ = []
instance (Typeable fieldName, HLCTypeable fieldType,
          GetStructFields xs) =>
         GetStructFields ( '( fieldName, fieldType ) ': xs) where
  getStructFields _ =
    StructField
    (getFieldName (Proxy :: Proxy fieldName))
    (fromTW (hlcType :: TW fieldType)) :
    getStructFields (Proxy :: Proxy xs)

generateStructFields :: forall structType.
                        (Struct structType,
                         GetStructFields (StructFields structType)) => Proxy structType -> [StructField]
generateStructFields _ = getStructFields (Proxy :: Proxy (StructFields structType))

type family GetFstElts (assocList :: [(*,*)]) :: [*] where
  GetFstElts '[] = '[]
  GetFstElts ( '(a , b) ': xs) = a ': GetFstElts xs

type GetFieldType structType fieldName = MapLookup fieldName ('Map (StructFields structType))

data IsAFieldOf structType fieldName
type InFstElts structType fieldName =
  If' (In fieldName (GetFstElts (StructFields structType)))
  ()
  (IsAFieldOf structType fieldName ~ ())

type ExprTy a = HLC (TypedExpr a)

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

getFieldName :: forall a. (Typeable a) => Proxy a -> SafeName
getFieldName _ = makeSafeName $ show $ typeRep (Proxy :: Proxy a)

readElt :: forall structType fieldName fieldType p.
           (StructFieldClass structType fieldName fieldType, Typeable fieldName) =>
           HLC (TypedExpr structType) ->
           Proxy fieldName ->
           HLC (TypedExpr fieldType)
readElt struct _ = do
  struct' <- struct
  return $ TypedExpr $ AccessPart (fromTypedExpr struct') $
    getFieldName (Proxy :: Proxy fieldName)

expVar :: HLCSymbol -> HLCExpr
expVar = LHSExpr . LHSVar

getFuncName :: (Typeable name, HLCFunction name argList retType) => Proxy name -> FuncName
getFuncName = FuncName . makeSafeName . show . typeRep

getStructName :: (Struct structType) => Proxy structType -> StructName
getStructName = StructName . makeSafeName . show . typeRep

getILType :: (Struct structType) => Proxy structType -> ILTypeName
getILType = ILTypeName . fromSafeName . fromStructName . getStructName

getObjType :: forall a. (HLCTypeable a) => a -> ILType
getObjType _ = fromTW (hlcType :: TW a)

structHLCType :: forall a. (Struct a) => TW a
structHLCType = TW $ BaseType NotConst
  (ILStructRef $ getILType (Proxy :: Proxy a))

emptyBlock = HLCBlock [] (StatementList []) NextLine

addVarToBlock :: Variable -> HLCBlock -> HLCBlock
addVarToBlock var (HLCBlock {..}) = HLCBlock {blockVars=var:blockVars,..}


nullPtr :: (HLCTypeable a) => HLC (TypedExpr (HLCPtr a))
nullPtr = return $ TypedExpr $ LitExpr $ IntLit 0

castPtr :: (RHSExpression void (HLCPtr HLCVoid),
           HLCTypeable a) => void -> HLC (TypedExpr (HLCPtr a))
castPtr ptr = do
  ptr' <- rhsExpr ptr
  return $ TypedExpr $ fromTypedExpr ptr'

