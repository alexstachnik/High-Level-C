
module HighLevelC.LangConstructs where

import Data.Typeable

import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes
import HighLevelC.CWriter
import Util.Names

ifThenElseRest :: TypedExpr HLCBool ->
                  (HLC Context -> HLC Context) ->
                  (HLC Context -> HLC Context) ->
                  HLC Context ->
                  HLC Context
ifThenElseRest cond trueBranch falseBranch rest = HLC $ do
  contSymb <- makeHLCSymbol_ $ makeSafeName "ifcont"
  trueBody <- grabBlock $ innerHLC $ trueBranch
    (return $ SomeContext contSymb)
  falseBody <- grabBlock $ innerHLC $ falseBranch
    (return $ SomeContext contSymb)
  writeStmt (IfThenElseRestStmt (fromTypedExpr cond) contSymb trueBody falseBody)
  innerHLC rest

ifThenElse :: TypedExpr HLCBool ->
              HLC Context ->
              HLC Context ->
              HLC Context
ifThenElse cond trueBranch falseBranch = HLC $ do
  trueBody <- grabBlock $ innerHLC trueBranch
  falseBody <- grabBlock $ innerHLC falseBranch
  writeStmt (IfThenElseStmt (fromTypedExpr cond) trueBody falseBody)
  return NextLine

whileRest :: TypedExpr HLCBool ->
             (HLC Context -> HLC Context -> HLC Context) ->
             HLC Context ->
             HLC Context
whileRest cond body rest = HLC $ do
  breakSymb <- makeHLCSymbol_ $ makeSafeName "whilebreak"
  contSymb <- makeHLCSymbol_ $ makeSafeName "whilecont"
  body <- grabBlock $ innerHLC $ body
    (return $ SomeContext breakSymb)
    (return $ SomeContext contSymb)
  writeStmt (WhileStmt (fromTypedExpr cond) breakSymb contSymb body)
  innerHLC rest
