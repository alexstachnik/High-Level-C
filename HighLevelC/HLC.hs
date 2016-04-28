{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module HighLevelC.HLC where

import Control.Monad.State
import Control.Monad.Writer(WriterT,execWriterT,MonadWriter,tell)
import Control.Monad.Identity(Identity(runIdentity))

import Language.C.Syntax.AST(CExtDecl)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq

import Data.Maybe(mapMaybe,listToMaybe)

import Data.Typeable

import Debug.Trace

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import HighLevelC.HLCTypes
import HighLevelC.CWriter
import Util.THUtil
import Util.Names
import IntermediateLang.ILTypes


newtype FuncNameTbl = FuncNameTbl {fromFuncNameTbl :: S.Set FuncBaseName}
                      deriving (Show)

data CodeState = CodeState {funcInstances :: FuncInstances,
                            structInstances :: StructInstances,
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

initState = CodeState {funcInstances = emptyFuncInstances,
                       structInstances = emptyStructInstances,
                       uidState = 0}

runInnerHLC :: HLC_ () -> CWriter
runInnerHLC c =
  runIdentity $ evalStateT (execWriterT $ runHLC_ c) initState

runOuterHLC :: HLC () -> CWriter
runOuterHLC = runInnerHLC . innerHLC

newUID :: HLC_ Integer
newUID = do
  (CodeState {..}) <- get
  put (CodeState {uidState=uidState+1,..})
  return uidState

makeHLCSymbol :: SafeName -> HLC HLCSymbol
makeHLCSymbol = HLC . makeHLCSymbol_

makeHLCSymbol_ :: SafeName -> HLC_ HLCSymbol
makeHLCSymbol_ safeName = do
  uid <- newUID
  return $ HLCSymbol uid safeName

lookupFunc :: FunctionInst -> HLC_ (Maybe HLCSymbol)
lookupFunc x = gets funcInstances >>= (return . M.lookup x . fromFuncInstances)

lookupStruct :: StructDef -> HLC_ Bool
lookupStruct x = gets structInstances >>= (return . S.member x. fromStructInstances)

addFuncInst :: FunctionInst -> HLCSymbol -> HLC_ ()
addFuncInst inst symb = modify $ \(CodeState {..}) ->
  CodeState {funcInstances = FuncInstances $ M.insert inst symb $ fromFuncInstances funcInstances,
             ..}

addStructInst :: StructDef -> HLC_ ()
addStructInst inst = modify $ \(CodeState {..}) ->
  CodeState {structInstances = StructInstances $ S.insert inst $ fromStructInstances structInstances,
             ..}

declareFuncInst :: FuncBaseName -> [ILType] -> HLC_ HLCSymbol
declareFuncInst baseName args = show args `trace` do
  let newName = deriveFuncName baseName args
  symb <- makeHLCSymbol_ newName
  addFuncInst (FunctionInst baseName args) symb
  return symb

declareStruct :: StructDef -> HLC_ ()
declareStruct struct = do
  exists <- lookupStruct struct
  case exists of
    True -> return ()
    False -> do
      addStructInst struct
      writeStruct struct

makeVar :: forall a. (HLCTypeable a) => SafeName -> HLC (TypedVar a)
makeVar name = HLC $ do
  let decl = structDef :: Maybe (TypedStructDef a)
  case decl of
    Just (TypedStructDef def) -> declareStruct def
    Nothing -> return ()
  symb <- makeHLCSymbol_ name
  writeVar $ Variable symb (fromTW (hlcType :: TW a)) Nothing
  return $ TypedVar symb

assignVar :: (HLCTypeable a) => TypedLHS a -> TypedExpr a -> HLC ()
assignVar lhs rhs = HLC $ writeStmt $
  HLCAssignment (helper lhs) (fromTypedExpr rhs)
  where helper :: TypedLHS b -> UntypedLHS
        helper (TypedLHSVar v) = LHSVar $ fromTypedVar v
        helper (TypedLHSDeref t) = LHSDeref $ helper t
        helper (TypedLHSDerefPlusOffset t offset) = LHSDerefPlusOffset (helper t) offset
        helper (TypedLHSElement st name) = LHSElement (helper st) (getFieldName name)

declareFunction :: forall a. (HLCTypeable a) =>
                   FuncBaseName -> [Argument] -> HLC (TypedExpr a) -> HLC HLCSymbol
declareFunction baseName args (HLC body) = HLC $ do
  symb <- declareFuncInst baseName $ map argumentType args
  let retType = fromTW (hlcType :: TW a)
  writeFuncProto $ FunctionProto retType symb args
  writeFunctionM symb args body
  return symb

getFunHLCSymbol :: FuncBaseName -> [ILType] -> HLC (Maybe HLCSymbol)
getFunHLCSymbol baseName args = HLC $
  lookupFunc $ FunctionInst baseName args

readElt :: forall structType fieldName fieldType.
           (Struct structType fieldName fieldType, Typeable fieldName) =>
           TypedExpr structType ->
           Proxy fieldName ->
           TypedExpr fieldType
readElt struct _ =
  TypedExpr $ AccessPart (fromTypedExpr struct) $
  getFieldName (Proxy :: Proxy fieldName)


makeStructField :: forall structType fieldName fieldType.
                   (Struct structType fieldName fieldType,
                    Typeable structType,
                    Typeable fieldName,
                    HLCTypeable fieldType) =>
                   Proxy fieldName ->
                   Maybe Integer ->
                   TypedStructField structType
makeStructField _ mArrLen =
  TypedStructField $
  StructField
  (getFieldName (Proxy :: Proxy fieldName))
  (fromTW (hlcType :: TW fieldType))
  mArrLen

makeStructDef :: forall structType.
                 (Typeable structType) =>
                 [TypedStructField structType] ->
                 TypedStructDef structType
makeStructDef =
  TypedStructDef .
  StructDef (getILType (Proxy :: Proxy structType)) . map fromTypedStructField
  
data Function a = Function a FuncBaseName
                | ExtFunc HLCSymbol



call0 :: forall b.
         (HLCTypeable b) =>
         Function (HLC (TypedExpr b)) ->
         HLC (TypedExpr b)
call0 (Function f baseName) = do
  mFSymb <- getFunHLCSymbol baseName []
  funcName <- case mFSymb of
    Just x -> return x
    Nothing -> do
      declareFunction
        baseName
        []
        f
  return $ TypedExpr $ FunctionCall (ExpVar funcName) []
call0 (ExtFunc name) = do
  return $ TypedExpr $ FunctionCall (ExpVar name) []


call1 :: forall a1 b.
         (HLCTypeable a1, HLCTypeable b) =>
         Function (TypedExpr a1 -> HLC (TypedExpr b)) ->
         TypedExpr a1 ->
         HLC (TypedExpr b)
call1 (Function f baseName) arg1 = do
  mFSymb <- getFunHLCSymbol baseName [getObjType arg1]
  funcName <- case mFSymb of
    Just x -> return x
    Nothing -> do
      argVar1 <- makeHLCSymbol $ makeSafeName "arg1"
      declareFunction
        baseName
        [Argument argVar1 (fromTW (hlcType :: TW a1))]
        (f
         (TypedExpr $ ExpVar argVar1))
  return $ TypedExpr $ FunctionCall (ExpVar funcName)
    [fromTypedExpr arg1]

call2 :: forall a1 a2 b.
         (HLCTypeable a1, HLCTypeable a2, HLCTypeable b) =>
         Function (TypedExpr a1 -> TypedExpr a2 -> HLC (TypedExpr b)) ->
         TypedExpr a1 ->
         TypedExpr a2 ->
         HLC (TypedExpr b)
call2 (Function f baseName) arg1 arg2 = do
  mFSymb <- getFunHLCSymbol baseName [getObjType arg1,
                                      getObjType arg2]
  funcName <- case mFSymb of
    Just x -> return x
    Nothing -> do
      argVar1 <- makeHLCSymbol $ makeSafeName "arg1"
      argVar2 <- makeHLCSymbol $ makeSafeName "arg2"
      declareFunction
        baseName
        [Argument argVar1 (fromTW (hlcType :: TW a1)),
         Argument argVar2 (fromTW (hlcType :: TW a2))]
        (f
         (TypedExpr $ ExpVar argVar1)
         (TypedExpr $ ExpVar argVar2))
  return $ TypedExpr $ FunctionCall (ExpVar funcName)
    [fromTypedExpr arg1,
     fromTypedExpr arg2]

