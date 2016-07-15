{-# LANGUAGE FlexibleContexts #-}

module Language.HLC.HighLevelC.LangConstructs where

import Data.Typeable

import Language.HLC.HighLevelC.HLC
import Language.HLC.HighLevelC.HLCTypes
import Language.HLC.HighLevelC.BasicTypes
import Language.HLC.HighLevelC.CWriter
import Language.HLC.Util.Names

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

whileStmt :: (RHSExpression a HLCBool) =>
         a ->
         (HLC Context -> HLC Context -> HLC Context) ->
         HLC ()
whileStmt cond body = HLC $ do
  breakSymb <- makeHLCSymbol_ $ makeSafeName "whilebreak"
  contSymb <- makeHLCSymbol_ $ makeSafeName "whilecont"
  body <- grabBlock $ innerHLC $ body
    (return $ SomeContext breakSymb)
    (return $ SomeContext contSymb)
  condExpr <- innerHLC $ rhsExpr cond
  writeStmt (WhileStmt (fromTypedExpr condExpr) breakSymb contSymb body)

