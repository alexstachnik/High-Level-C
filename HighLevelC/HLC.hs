{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module HighLevelC.HLC where

import Control.Monad.State
import Control.Monad.Writer(WriterT,execWriterT,MonadWriter,tell,listen)
import Control.Monad.Identity(Identity(runIdentity))

import Language.C.Syntax.AST(CExtDecl)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq

import GHC.TypeLits

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



initState = CodeState {funcInstances = M.empty,
                       structInstances = M.empty,
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

lookupFunc :: FuncName -> HLC_ (Maybe HLCSymbol)
lookupFunc x = gets funcInstances >>= (return . M.lookup x)

lookupStruct :: StructName -> HLC_ (Maybe HLCSymbol)
lookupStruct x = gets structInstances >>= (return . M.lookup x)

addFuncInst :: FuncName -> HLCSymbol -> HLC_ ()
addFuncInst name symb = modify $ \(CodeState {..}) ->
  CodeState {funcInstances = M.insert name symb funcInstances,..}

addStructInst :: StructName -> HLCSymbol -> HLC_ ()
addStructInst name symb = modify $ \(CodeState {..}) ->
  CodeState {structInstances = M.insert name symb $ structInstances,..}

makeFuncSymb :: (HLCFunction name ty retType) =>
                Proxy name -> HLC_ HLCSymbol
makeFuncSymb name = do
  symb <- makeHLCSymbol_ (fromFuncName $ getFuncName name)
  addFuncInst (getFuncName name) symb
  return symb

makeStructSymb :: (Struct p structType) =>
                  Proxy structType -> HLC_ HLCSymbol
makeStructSymb name = do
  let symb = ExactSymbol $ fromSafeName $ fromStructName $ getStructName name
  addStructInst (getStructName name) symb
  return symb

declareFunc :: forall name ty retType r. (HLCFunction name ty retType) =>
               Proxy name -> [Argument] -> HLC (TypedExpr retType) -> HLC_ HLCSymbol
declareFunc proxyName args (HLC func) = do
  symb <- makeFuncSymb proxyName
  let retType = fromTW (hlcType :: TW retType)
  block <- grabBlock func
  writeFuncProto $ FunctionProto retType symb args
  writeFunc $ FunctionDef retType symb args block
  return symb
    where name = getFuncName proxyName

makeStructField :: forall p structType fieldName fieldType arrLen.
                   (KnownNat arrLen,
                    StructFieldClass p structType fieldName fieldType arrLen) =>
                   Proxy structType -> Proxy fieldName -> HLC_ (TypedVar fieldType)
makeStructField structName fieldName = do
  let arrLen = case natVal (Proxy :: Proxy arrLen) of
        1 -> Nothing
        n -> Just n
  writeStructVarDecl $
    StructField (getFieldName fieldName) (fromTW (hlcType :: TW fieldType)) arrLen
  return $ TypedVar $ ExactSymbol $ fromSafeName $ getFieldName fieldName

declareStruct :: forall p structType. (Struct p structType) =>
                 Proxy structType -> HLC_ HLCSymbol
declareStruct proxyName = do
  symb <- makeStructSymb proxyName
  writeStructProto $ StructProto symb
  fields <- grabStructVars $ innerHLC $
    constructor (Proxy :: Proxy structType) (HLC . makeStructField (Proxy :: Proxy structType))
  writeStruct $ StructDef symb fields
  return symb

callFunc :: forall name ty retType. (HLCFunction name ty retType) =>
            Proxy name -> [(Argument, HLCExpr)] -> HLC (TypedExpr retType) -> HLC (TypedExpr retType)
callFunc proxyName args f = HLC $ do
  msymb <- lookupFunc name
  symb <- case msymb of
    (Just symb) -> return symb
    Nothing -> declareFunc proxyName (map fst args) f
  return $ TypedExpr $ FunctionCall (expVar symb) (map snd args)
  where name = getFuncName proxyName
