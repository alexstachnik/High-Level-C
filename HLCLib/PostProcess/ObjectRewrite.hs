{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module PostProcess.ObjectRewrite where

import Control.Monad.State
import Control.Monad.Identity(Identity(runIdentity))

import qualified Data.Map as M

import HighLevelC.HLC
import HighLevelC.HLCCalls
import HighLevelC.HLCTypes
import HighLevelC.CWriter
import HighLevelC.BasicTypes
import HighLevelC.PrimFunctions
import HighLevelC.VarDecls
import HighLevelC.Operators
import HighLevelC.TypeSynonyms
import HighLevelC.LangConstructs
import Util.Names

import Debug.Trace

newtype CounterM a = CounterM {runCounterM :: (StateT Int Identity) a}
                   deriving (Monad,Applicative,Functor,MonadState Int)

newSymbol :: CounterM HLCSymbol
newSymbol = do
  n <- get
  put (n+1)
  return $ ExactSymbol ("label__" ++ show n)

processObjects :: CWriter -> CWriter
processObjects (CWriter {..})=
  (CWriter {funcDefs = evalState (runCounterM $ mapM procFunc funcDefs) 0,..})

endOfFunctionSymb = ExactSymbol "label__endOfFunction"

procFunc :: FunctionDef -> CounterM FunctionDef
procFunc (FunctionDef {..}) = do
  s <- newSymbol
  newBlock <- procBlock [] M.empty (M.singleton endOfFunctionSymb s) fdefBody
  return (FunctionDef {fdefBody = appendLabel s newBlock,..})

procStmt :: [HLCBlock] ->
            M.Map HLCSymbol [HLCBlock] ->
            M.Map HLCSymbol HLCSymbol ->
            HLCStatement ->
            CounterM [HLCStatement]
procStmt allDestrs destrMap symbMap stmt =
  case stmt of
    (BlockStmt block) -> do
      newBlock <- procBlock allDestrs destrMap symbMap block
      return [BlockStmt newBlock]
    (IfThenElseRestStmt cond cont thenBlock elseBlock) -> do
      thenBlock' <- procBlock allDestrs destrMap symbMap thenBlock
      elseBlock' <- procBlock allDestrs destrMap symbMap elseBlock
      return [IfThenElseRestStmt cond cont thenBlock' elseBlock']
    (IfThenElseStmt cond thenBlock elseBlock) -> do
      thenBlock' <- procBlock allDestrs destrMap symbMap thenBlock
      elseBlock' <- procBlock allDestrs destrMap symbMap elseBlock
      return [IfThenElseStmt cond thenBlock' elseBlock']
    (WhileStmt cond breakSymb contSymb block) -> do
      newContSymb <- newSymbol
      newBreakSymb <- newSymbol
      let newDestrMap =
            M.insert contSymb [] $
            M.insert breakSymb [] destrMap
          newSymbMap =
            M.insert contSymb newContSymb $
            M.insert breakSymb newBreakSymb symbMap
      newBlock <- procBlock' allDestrs newDestrMap newSymbMap block
      let newStmts = StatementList $ (++ [LabelStmt newContSymb]) $
            fromStatementList $ blockStmts newBlock
      return [WhileStmt cond breakSymb contSymb (newBlock {blockStmts = newStmts}),
              LabelStmt newBreakSymb]
    _ -> return [stmt]


f1 (ExactSymbol "label__17") = "HEYA"
f1 _ = ""
f2 (ExactSymbol "label__17") = "HEYB"
f2 _ = ""
f3 (ExactSymbol "label__17") = "HEYC"
f3 _ = ""

procBlock :: [HLCBlock] ->
             M.Map HLCSymbol [HLCBlock] ->
             M.Map HLCSymbol HLCSymbol ->
             HLCBlock ->
             CounterM HLCBlock
procBlock allDestrs destrMap symbMap block =
  case blockRetCxt block of
    (SomeContext symb) -> do
      case M.member symb symbMap of
        True -> procBlock' allDestrs destrMap symbMap block
        False -> do
          s <- newSymbol
          newBlock <- procBlock' allDestrs (M.insert symb [] destrMap) (M.insert symb s symbMap) block
          return $ appendLabel s newBlock
    _ -> procBlock' allDestrs destrMap symbMap block
      

procBlock' :: [HLCBlock] ->
              M.Map HLCSymbol [HLCBlock] ->
              M.Map HLCSymbol HLCSymbol ->
              HLCBlock ->
              CounterM HLCBlock
procBlock' allDestrs destrMap symbMap (HLCBlock {..}) = do
  constrs <- mapM (procConsDestr . variableCons) blockVars
  let destrs = map variableDest blockVars
      (StatementList stmts) = blockStmts
      newDestrMap = M.map (destrs ++) destrMap
      newDestrList = destrs ++ allDestrs
  newBody <- mapM (procStmt newDestrList newDestrMap symbMap) $ fromStatementList blockStmts
  fullDestrList <- case blockRetCxt of
    (NullContext var expr) -> do
      blocks <- mapM procConsDestr newDestrList
      return (map BlockStmt blocks ++
              case expr of
                Void -> [JumpStmt (symbMap M.! endOfFunctionSymb)]
                _ -> [AssignmentStmt (LHSVar $ variableName var) expr,
                      JumpStmt (symbMap M.! endOfFunctionSymb)])
    VoidReturn -> do
      blocks <- mapM procConsDestr newDestrList
      return (map BlockStmt blocks ++
                [JumpStmt (symbMap M.! endOfFunctionSymb)])
    (SomeContext retPtr) -> do
      blocks <- case M.lookup retPtr newDestrMap of
        Just blocks -> mapM procConsDestr (newDestrMap M.! retPtr)
        Nothing -> return []
      return (map BlockStmt blocks  ++
              [JumpStmt (symbMap M.! retPtr)])
    NextLine -> do
      blocks <- mapM procConsDestr destrs
      return $ map BlockStmt blocks
  return (HLCBlock {blockStmts =
                    StatementList (map BlockStmt constrs ++
                                   concat newBody ++
                                   fullDestrList),
                    ..})

appendLabel :: HLCSymbol -> HLCBlock -> HLCBlock
appendLabel s block =
  let newStmtList = StatementList $ (\x -> (x ++ [LabelStmt s])) $ fromStatementList $ blockStmts block
      result = block {blockStmts = newStmtList} in
  result

procConsDestr :: HLCBlock ->
                 CounterM HLCBlock
procConsDestr block@(HLCBlock {..}) =
  case blockRetCxt of
    (SomeContext constrCont) -> do
      s <- newSymbol
      newBlock <- procBlock [] M.empty (M.singleton constrCont s) block
      return $ appendLabel s newBlock
    x -> return block

