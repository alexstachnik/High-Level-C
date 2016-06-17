{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HighLevelC.HLCCalls where

import Data.Typeable

import HighLevelC.CWriter
import HighLevelC.HLC
import HighLevelC.HLCTypes
import Util.Names

callExt0 :: (HLCTypeable b) =>
             ExtFunction b ->
             VarArg ->
             TypedExpr b
callExt0 (ExtFunction name) NilArg =
  TypedExpr $ FunctionCall (LHSExpr $ LHSVar name) []
callExt0 (VarExtFunction name) varArgs =
  TypedExpr $ FunctionCall (LHSExpr $ LHSVar name)
  (varArgToList varArgs)


callExt1 :: (HLCTypeable b,
             HLCTypeable a1, Passability a1 ~ IsPassable) =>
             ExtFunction (a1 -> b) ->
             TypedExpr a1 ->
             VarArg ->
             TypedExpr b
callExt1 (ExtFunction name) arg1 NilArg =
  TypedExpr $ FunctionCall (LHSExpr $ LHSVar name) [fromTypedExpr arg1]
callExt1 (VarExtFunction name) arg1 varArgs =
  TypedExpr $ FunctionCall (LHSExpr $ LHSVar name)
  ([fromTypedExpr arg1] ++ varArgToList varArgs)

callExt2 :: (HLCTypeable b,
             HLCTypeable a1, Passability a1 ~ IsPassable,
             HLCTypeable a2, Passability a2 ~ IsPassable) =>
             ExtFunction (a1 -> a2 -> b) ->
             TypedExpr a1 ->
             TypedExpr a2 ->
             VarArg ->
             TypedExpr b
callExt2 (ExtFunction name) arg1 arg2 NilArg =
  TypedExpr $ FunctionCall (LHSExpr $ LHSVar name) [fromTypedExpr arg1, fromTypedExpr arg2]
callExt2 (VarExtFunction name) arg1 arg2 varArgs =
  TypedExpr $ FunctionCall (LHSExpr $ LHSVar name)
  ([fromTypedExpr arg1, fromTypedExpr arg2] ++ varArgToList varArgs)

newtype ArgWrap0 r = ArgWrap0 {fromArgWrap0 :: r}
call0 :: forall name a1 retType.
         (HLCFunction
          name
          ArgWrap0
          retType,
          HLCTypeable retType) =>
         Proxy name ->
         HLC (TypedExpr retType)
call0 proxyName = do
  let argFields = []
      untypedArgs = []
  callFunc proxyName (zip argFields untypedArgs)
    (fromArgWrap0 $ call proxyName (returnContext proxyName))
  
newtype ArgWrap1 a r = ArgWrap1 {fromArgWrap1 :: a -> r}
call1 :: forall name a1 retType.
         (HLCFunction
          name
          (ArgWrap1 (HLC (TypedExpr a1)))
          retType,
          HLCTypeable a1, HLCTypeable retType,
          Passability a1 ~ IsPassable) =>
         Proxy name ->
         HLC (TypedExpr a1) ->
         HLC (TypedExpr retType)
call1 proxyName marg1 = do
  arg1 <- marg1
  [d1] <- mapM (makeHLCSymbol . makeSafeName) ["d1"]
  let argFields = [Argument d1 (getObjType arg1)]
      untypedArgs = [fromTypedExpr arg1]
  callFunc proxyName (zip argFields untypedArgs)
    ((fromArgWrap1 $ call proxyName (returnContext proxyName))
     (return $ TypedExpr $ LHSExpr $ LHSVar d1))

returnContext :: (HLCFunction funcName args retType) =>
                 Proxy funcName -> HLC (TypedExpr retType) -> HLC Context
returnContext proxyName retVal = do
  (Just retVar) <- HLC $ lookupFuncRetVar $ getFuncName proxyName
  retVal' <- retVal
  return $ NullContext retVar $ fromTypedExpr retVal'
  

newtype ArgWrap2 a1 a2 r = ArgWrap2 {fromArgWrap2 :: a1 -> a2 -> r}
call2 :: forall name a1 a2 retType.
         (HLCFunction
          name
          (ArgWrap2 (HLC (TypedExpr a1)) (HLC (TypedExpr a2)))
          retType,
          HLCTypeable a1, HLCTypeable a2, HLCTypeable retType,
          Passability a1 ~ IsPassable,
          Passability a2 ~ IsPassable) =>
         Proxy name ->
         HLC (TypedExpr a1) ->
         HLC (TypedExpr a2) ->
         HLC (TypedExpr retType)
call2 proxyName marg1 marg2 = do
  arg1 <- marg1
  arg2 <- marg2
  [d1,d2] <- mapM (makeHLCSymbol . makeSafeName) ["d1","d2"]
  let argFields = [Argument d1 (getObjType arg1),
                   Argument d2 (getObjType arg2)]
      untypedArgs = [fromTypedExpr arg1,
                     fromTypedExpr arg2]
  callFunc proxyName (zip argFields untypedArgs)
    ((fromArgWrap2 $ call proxyName (returnContext proxyName))
     (return $ TypedExpr $ LHSExpr $ LHSVar d1)
     (return $ TypedExpr $ LHSExpr $ LHSVar d2))

