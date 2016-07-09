{-# LANGUAGE UndecidableInstances #-}
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

makeStructSymb :: (Struct structType) =>
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
  case (null args) && (retType == fromTW (hlcType :: TW HLCVoid)) of
    False -> writeFunc $ FunctionDef retType symb args (addVarToBlock retVar block) retVar
    True -> writeFunc $ FunctionDef retType symb args (block {blockRetCxt = VoidReturn}) retVar
  return symb
    where name = getFuncName proxyName

makeStructField :: forall structType fieldName fieldType.
                   (StructFieldClass structType fieldName fieldType) =>
                   Proxy structType -> Proxy fieldName -> HLC_ (TypedLHS fieldType)
makeStructField structName fieldName = do
  writeStructVarDecl $
    StructField (getFieldName fieldName) (fromTW (hlcType :: TW fieldType))
  return $ TypedLHSVar $ TypedVar $ ExactSymbol $ fromSafeName $ getFieldName fieldName

declareStruct' :: forall structType. (Struct structType, GetStructFields (StructFields structType)) =>
                  Proxy structType -> HLC_ ()
declareStruct' proxyName = do
  symb <- makeStructSymb proxyName
  writeStructProto $ StructProto symb
  writeStruct $ StructDef symb (generateStructFields (Proxy :: Proxy structType))
  return ()

declareStruct :: forall structType. (Struct structType, GetStructFields (StructFields structType)) =>
                  Proxy structType -> HLC ()
declareStruct proxyName = HLC $ do
  msymb <- lookupStruct (getStructName (Proxy :: Proxy structType))
  case msymb of
    (Just symb) -> return ()
    Nothing -> declareStruct' (Proxy :: Proxy structType)

instance (Struct structType,
          GetStructFields (StructFields structType),
          StructFieldConstructors structType (StructFields structType),
          StructFieldDestructors structType (StructFields structType)) =>
         Instanciable structType False where
  declareObj' _ = declareStruct (Proxy :: Proxy structType)
  construct' _ this ret = do
    callSubConstructors (Proxy :: Proxy '(structType,StructFields structType)) this
    constructor (Proxy :: Proxy structType) this ret
  destruct' _ this ret = do
    callSubDestructors (Proxy :: Proxy '(structType,StructFields structType)) this
    destructor (Proxy :: Proxy structType) this ret

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

withType :: (HLCTypeable a) => HLC (TypedExpr a)
withType = return undefined

exprStmt :: HLC (TypedExpr a) -> HLC ()
exprStmt = HLC . (>>= (writeStmt . ExpStmt . fromTypedExpr)) . innerHLC

class (Struct structType) => StructFieldConstructors structType (fieldPairs :: [(*,*)]) where
  callSubConstructors :: Proxy '(structType,fieldPairs) -> HLC (TypedLHS structType) -> HLC ()
instance (Struct structType) => StructFieldConstructors structType '[] where
  callSubConstructors _ _ = return ()
instance (Typeable fieldName, Instanciable fieldType (IsPrimitive fieldType),
          StructFieldConstructors structType xs,
          StructFieldClass structType fieldName fieldType) =>
         StructFieldConstructors structType ( '( fieldName, fieldType ) ': xs) where
  callSubConstructors _ this = do
    consCont <- makeHLCSymbol $ makeSafeName "subconscont"
    x <- this
    construct (Proxy :: Proxy fieldType) (return $ TypedLHSElement x (Proxy :: Proxy fieldName)) (return $ SomeContext consCont)
    callSubConstructors (Proxy :: Proxy '(structType,xs)) this

class (Struct structType) => StructFieldDestructors structType (fieldPairs :: [(*,*)]) where
  callSubDestructors :: Proxy '(structType,fieldPairs) -> HLC (TypedLHS structType) -> HLC ()
instance (Struct structType) => StructFieldDestructors structType '[] where
  callSubDestructors _ _ = return ()
instance (Typeable fieldName, Instanciable fieldType (IsPrimitive fieldType),
          StructFieldDestructors structType xs,
          StructFieldClass structType fieldName fieldType) =>
         StructFieldDestructors structType ( '( fieldName, fieldType ) ': xs) where
  callSubDestructors _ this = do
    destCont <- makeHLCSymbol $ makeSafeName "subdestcont"
    x <- this
    destruct (Proxy :: Proxy fieldType) (return $ TypedLHSElement x (Proxy :: Proxy fieldName)) (return $ SomeContext destCont)
    callSubDestructors (Proxy :: Proxy '(structType,xs)) this
