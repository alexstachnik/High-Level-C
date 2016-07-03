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


callExt0 :: (HLCTypeable r) =>
             ExtFunction (r) ->
             VarArg ->
             HLC (TypedExpr r)
callExt0 func varArgs = do
  HLC $ mapM_ writePreproDir $ extFunctionDirs func
  return $ TypedExpr $ FunctionCall (LHSExpr $ LHSVar $ extFunctionName func) 
    ([] ++
     case func of
       ExtFunction _ _ -> []
       VarExtFunction _ _ -> varArgToList varArgs)


callExt1 :: (HLCTypeable r,
             RHSExpression a1 b1,
             HLCTypeable b1, Passability b1 ~ IsPassable) =>
             ExtFunction (b1 -> r) ->
             a1 ->
             VarArg ->
             HLC (TypedExpr r)
callExt1 func arg1 varArgs = do
  arg1' <- rhsExpr arg1
  HLC $ mapM_ writePreproDir $ extFunctionDirs func
  return $ TypedExpr $ FunctionCall (LHSExpr $ LHSVar $ extFunctionName func) 
    ([fromTypedExpr arg1'] ++
     case func of
       ExtFunction _ _ -> []
       VarExtFunction _ _ -> varArgToList varArgs)


callExt2 :: (HLCTypeable r,
             RHSExpression a1 b1,
             RHSExpression a2 b2,
             HLCTypeable b1, Passability b1 ~ IsPassable,
             HLCTypeable b2, Passability b2 ~ IsPassable) =>
             ExtFunction (b1 -> b2 -> r) ->
             a1 ->
             a2 ->
             VarArg ->
             HLC (TypedExpr r)
callExt2 func arg1 arg2 varArgs = do
  arg1' <- rhsExpr arg1
  arg2' <- rhsExpr arg2
  HLC $ mapM_ writePreproDir $ extFunctionDirs func
  return $ TypedExpr $ FunctionCall (LHSExpr $ LHSVar $ extFunctionName func) 
    ([fromTypedExpr arg1',fromTypedExpr arg2'] ++
     case func of
       ExtFunction _ _ -> []
       VarExtFunction _ _ -> varArgToList varArgs)


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
          (ArgWrap1 (TypedLHS a1))
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
     (TypedLHSVar $ TypedVar d1))

returnContext :: (HLCFunction funcName args retType) =>
                 Proxy funcName ->
                 (forall a. (RHSExpression a retType) => a -> HLC Context)
returnContext proxyName retVal = do
  (Just retVar) <- HLC $ lookupFuncRetVar $ getFuncName proxyName
  retVal' <- rhsExpr retVal
  return $ NullContext retVar $ fromTypedExpr retVal'
  

newtype ArgWrap2 a1 a2 r = ArgWrap2 {fromArgWrap2 :: a1 -> a2 -> r}
call2 :: forall name a1 a2 retType.
         (HLCFunction
          name
          (ArgWrap2 (TypedLHS a1) (TypedLHS a2))
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
     (TypedLHSVar $ TypedVar d1)
     (TypedLHSVar $ TypedVar d2))

newtype ArgWrap3 a1 a2 a3 r = ArgWrap3 {fromArgWrap3 :: a1 -> a2 -> a3 -> r}
call3 :: forall name a1 a2 a3 retType.
         (HLCFunction
          name
          (ArgWrap3 (TypedLHS a1) (TypedLHS a2) (TypedLHS a3))
          retType,
          HLCTypeable a1, HLCTypeable a2, HLCTypeable a3,
          HLCTypeable retType,
          Passability a1 ~ IsPassable,
          Passability a2 ~ IsPassable,
          Passability a3 ~ IsPassable) =>
         Proxy name ->
         HLC (TypedExpr a1) ->
         HLC (TypedExpr a2) ->
         HLC (TypedExpr a3) ->
         HLC (TypedExpr retType)
call3 proxyName marg1 marg2 marg3 = do
  arg1 <- marg1
  arg2 <- marg2
  arg3 <- marg3
  [d1,d2,d3] <- mapM (makeHLCSymbol . makeSafeName) ["d1","d2","d3"]
  let argFields = [Argument d1 (getObjType arg1),
                   Argument d2 (getObjType arg2),
                   Argument d3 (getObjType arg3)]
      untypedArgs = [fromTypedExpr arg1,
                     fromTypedExpr arg2,
                     fromTypedExpr arg3]
  callFunc proxyName (zip argFields untypedArgs)
    ((fromArgWrap3 $ call proxyName (returnContext proxyName))
     (TypedLHSVar $ TypedVar d1)
     (TypedLHSVar $ TypedVar d2)
     (TypedLHSVar $ TypedVar d3))

