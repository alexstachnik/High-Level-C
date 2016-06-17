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
lookupFunc x = gets funcInstances >>= (return . (fmap fst) . M.lookup x)

lookupFuncRetVar :: FuncName -> HLC_ (Maybe Variable)
lookupFuncRetVar x = gets funcInstances >>= (return . (fmap snd) . M.lookup x)

lookupStruct :: StructName -> HLC_ (Maybe HLCSymbol)
lookupStruct x = gets structInstances >>= (return . M.lookup x)

addFuncInst :: FuncName -> (HLCSymbol,Variable) -> HLC_ ()
addFuncInst name pair = modify $ \(CodeState {..}) ->
  CodeState {funcInstances = M.insert name pair funcInstances,..}

addStructInst :: StructName -> HLCSymbol -> HLC_ ()
addStructInst name symb = modify $ \(CodeState {..}) ->
  CodeState {structInstances = M.insert name symb $ structInstances,..}

makeFuncSymb :: (HLCFunction name ty retType) =>
                Proxy name -> Variable -> HLC_ HLCSymbol
makeFuncSymb name var = do
  let symb = ExactSymbol $ fromSafeName $ fromFuncName $ getFuncName name
  addFuncInst (getFuncName name) (symb,var)
  return symb

makeStructSymb :: (Struct p structType) =>
                  Proxy structType -> HLC_ HLCSymbol
makeStructSymb name = do
  let symb = ExactSymbol $ fromSafeName $ fromStructName $ getStructName name
  addStructInst (getStructName name) symb
  return symb

declareFunc' :: forall name ty retType r. (HLCFunction name ty retType) =>
                Proxy name -> [Argument] -> HLC Context -> HLC_ HLCSymbol
declareFunc' proxyName args (HLC func) = do
  retVariableName <- makeHLCSymbol_ (SafeName "returnVariable")
  let retType = fromTW (hlcType :: TW retType)
      retVar = Variable retVariableName retType Nothing emptyBlock emptyBlock
  symb <- makeFuncSymb proxyName retVar
  block <- grabBlock func
  writeFuncProto $ FunctionProto retType symb args
  case args of
    (_:_) -> writeFunc $ FunctionDef retType symb args (addVarToBlock retVar block)
    [] -> writeFunc $ FunctionDef retType symb args block
  return symb
    where name = getFuncName proxyName

makeStructField :: forall p structType fieldName fieldType.
                   (StructFieldClass p structType fieldName fieldType) =>
                   Proxy structType -> Proxy fieldName -> HLC_ (TypedLHS fieldType)
makeStructField structName fieldName = do
  writeStructVarDecl $
    StructField (getFieldName fieldName) (fromTW (hlcType :: TW fieldType))
  return $ TypedLHSVar $ TypedVar $ ExactSymbol $ fromSafeName $ getFieldName fieldName

declareStruct' :: forall p structType. (Struct p structType) =>
                  Proxy structType -> HLC_ HLCSymbol
declareStruct' proxyName = do
  symb <- makeStructSymb proxyName
  writeStructProto $ StructProto symb
  fields <- grabStructVars $ innerHLC $
    constructor
    (Proxy :: Proxy structType)
    (HLC . makeStructField (Proxy :: Proxy structType))
    VoidReturn
  writeStruct $ StructDef symb fields
  return symb

declareStruct :: forall p structType. (Struct p structType) =>
                  Proxy structType -> HLC HLCSymbol
declareStruct proxyName = HLC $ do
  msymb <- lookupStruct (getStructName (Proxy :: Proxy structType))
  case msymb of
    (Just symb) -> return symb
    Nothing -> declareStruct' (Proxy :: Proxy structType)

declareFunc :: forall name ty retType r. (HLCFunction name ty retType) =>
               Proxy name -> [Argument] -> HLC Context -> HLC HLCSymbol
declareFunc proxyName args f = HLC $ do
  msymb <- lookupFunc (getFuncName proxyName)
  case msymb of
    (Just symb) -> return symb
    Nothing -> declareFunc' proxyName args f

callFunc :: forall name ty retType. (HLCFunction name ty retType) =>
            Proxy name -> [(Argument, HLCExpr)] -> HLC Context -> HLC (TypedExpr retType)
callFunc proxyName args f = do
  symb <- declareFunc proxyName (map fst args) f
  return $ TypedExpr $ FunctionCall (expVar symb) (map snd args)
  where name = getFuncName proxyName

withType :: (HLCTypeable a) => HLC a
withType = return undefined

exprStmt :: HLC (TypedExpr a) -> HLC ()
exprStmt = HLC . (>>= (writeStmt . ExpStmt . fromTypedExpr)) . innerHLC
