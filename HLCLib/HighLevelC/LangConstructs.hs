{-# LANGUAGE FlexibleContexts #-}

module HighLevelC.LangConstructs where

import Data.Typeable

import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes
import HighLevelC.CWriter
import Util.Names

ifThenElseRest :: (RHSExpression a HLCBool) =>
                  a ->
                  (HLC Context -> HLC Context) ->
                  (HLC Context -> HLC Context) ->
                  HLC ()
ifThenElseRest cond trueBranch falseBranch = HLC $ do
  contSymb <- makeHLCSymbol_ $ makeSafeName "ifcont"
  trueBody <- grabBlock $ innerHLC $ trueBranch
    (return $ SomeContext contSymb)
  falseBody <- grabBlock $ innerHLC $ falseBranch
    (return $ SomeContext contSymb)
  condExpr <- innerHLC $ rhsExpr cond
  writeStmt (IfThenElseRestStmt (fromTypedExpr condExpr) contSymb trueBody falseBody)

ifThenElse :: (RHSExpression a HLCBool) =>
              a ->
              HLC Context ->
              HLC Context ->
              HLC Context
ifThenElse cond trueBranch falseBranch = HLC $ do
  trueBody <- grabBlock $ innerHLC trueBranch
  falseBody <- grabBlock $ innerHLC falseBranch
  condExpr <- innerHLC $ rhsExpr cond
  writeStmt (IfThenElseStmt (fromTypedExpr condExpr) trueBody falseBody)
  return NextLine

whileRest :: (RHSExpression a HLCBool) =>
             a ->
             (HLC Context -> HLC Context -> HLC Context) ->
             HLC ()
whileRest cond body = HLC $ do
  breakSymb <- makeHLCSymbol_ $ makeSafeName "whilebreak"
  contSymb <- makeHLCSymbol_ $ makeSafeName "whilecont"
  body <- grabBlock $ innerHLC $ body
    (return $ SomeContext breakSymb)
    (return $ SomeContext contSymb)
  condExpr <- innerHLC $ rhsExpr cond
  writeStmt (WhileStmt (fromTypedExpr condExpr) breakSymb contSymb body)

