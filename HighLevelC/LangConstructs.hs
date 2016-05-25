
module HighLevelC.LangConstructs where

import Data.Typeable

import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes
import HighLevelC.CWriter
import Util.Names

ifThenElseRest :: TypedExpr HLCBool ->
                  (HLC (TypedExpr b) -> HLC (TypedExpr b)) ->
                  (HLC (TypedExpr b) -> HLC (TypedExpr b)) ->
                  HLC (TypedExpr b) ->
                  HLC (TypedExpr b)
ifThenElseRest cond trueBranch falseBranch rest = HLC $ do
  trueBody <- grabBlock $ innerHLC $ trueBranch (return $ TypedExpr Void)
  falseBody <- grabBlock $ innerHLC $ falseBranch (return $ TypedExpr Void)
  restBody <- grabBlock $ innerHLC rest
  writeStmt (IfThenElseStmt (fromTypedExpr cond) trueBody falseBody)
  writeStmt $ BlockStmt restBody
  return $ TypedExpr Void

ifThenElse :: TypedExpr HLCBool ->
              HLC (TypedExpr b) ->
              HLC (TypedExpr b) ->
              HLC (TypedExpr b)
ifThenElse cond trueBranch falseBranch = HLC $ do
  trueBody <- grabBlock $ innerHLC trueBranch
  falseBody <- grabBlock $ innerHLC falseBranch
  writeStmt (IfThenElseStmt (fromTypedExpr cond) trueBody falseBody)
  return $ TypedExpr Void

breakStmt :: HLCSymbol -> HLC (TypedExpr b)
breakStmt symb = HLC $ do
  writeStmt (GotoStmt symb)
  return $ TypedExpr Void

whileRest :: TypedExpr HLCBool ->
             (HLC (TypedExpr b) -> HLC (TypedExpr b)) ->
             HLC (TypedExpr b) ->
             HLC (TypedExpr b)
whileRest cond body rest = HLC $ do
  breakSymb <- makeHLCSymbol_ $ makeSafeName "whilebreak"
  body <- grabBlock $ innerHLC $ body (breakStmt breakSymb)
  rest <- grabBlock $ innerHLC rest
  writeStmt (WhileStmt (fromTypedExpr cond) body)
  writeStmt $ BlockStmt rest
  return $ TypedExpr Void
