{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HighLevelC.HLCCalls where

import Data.Typeable

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

call0 :: forall name retType.
         (HLCFunction
          (name)
          (HLC (TypedExpr retType))
          retType,
          HLCTypeable retType) =>
         Proxy (name) ->
         HLC (TypedExpr retType)
call0 proxyName = do
  let argFields = []
      untypedArgs = []
  callFunc proxyName (zip argFields untypedArgs) (call proxyName)
  

call1 :: forall name a1 retType.
         (HLCFunction
          (name a1)
          ((TypedExpr a1) -> HLC (TypedExpr retType))
          retType,
          HLCTypeable a1, HLCTypeable retType,
          Passability a1 ~ IsPassable) =>
         Proxy (name a1) ->
         TypedExpr a1 ->
         HLC (TypedExpr retType)
call1 proxyName arg1 = do
  [d1] <- mapM (makeHLCSymbol . makeSafeName) ["d1"]
  let argFields = [Argument d1 (getObjType arg1) Nothing]
      untypedArgs = [fromTypedExpr arg1]
  callFunc proxyName (zip argFields untypedArgs) ((call proxyName) arg1)
  

call2 :: forall name a1 a2 retType.
         (HLCFunction
          (name a1 a2)
          ((TypedExpr a1) -> (TypedExpr a2) -> HLC (TypedExpr retType))
          retType,
          HLCTypeable a1, HLCTypeable a2, HLCTypeable retType,
          Passability a1 ~ IsPassable, Passability a2 ~ IsPassable) =>
         Proxy (name a1 a2) ->
         TypedExpr a1 ->
         TypedExpr a2 ->
         HLC (TypedExpr retType)
call2 proxyName arg1 arg2 = do
  [d1,d2] <- mapM (makeHLCSymbol . makeSafeName) ["d1","d2"]
  let argFields = [Argument d1 (getObjType arg1) Nothing,
                   Argument d2 (getObjType arg2) Nothing]
      untypedArgs = [fromTypedExpr arg1,
                     fromTypedExpr arg2]
  callFunc proxyName (zip argFields untypedArgs) ((call proxyName) arg1 arg2)
  

