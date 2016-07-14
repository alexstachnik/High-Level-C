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


