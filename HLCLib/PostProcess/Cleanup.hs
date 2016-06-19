{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module PostProcess.Cleanup where

import qualified Data.Sequence as Sq
import qualified Data.Set as S

import Data.Maybe
import Data.Data
import Data.Typeable

import HighLevelC.HLCTypes
import HighLevelC.CWriter
import Util.Names

import Debug.Trace

cleanup :: CWriter -> CWriter
cleanup (CWriter {..}) =
  CWriter {funcDefs =
              cleanEmptyBlocks $
              cleanNoopJumps $
              fromJust $ cleanLbls (S.fromList $ getJumps funcDefs) funcDefs,..}

cleanEmptyBlocks :: forall b. (Data b) => b -> b
cleanEmptyBlocks elt =
  case eqT of
    (Just (Refl :: StatementList :~: b)) ->
      let stmts = fromStatementList elt in
      StatementList $ mapMaybe cleanBlockStmt stmts
    Nothing ->
      gmapT cleanEmptyBlocks elt

cleanBlockStmt :: HLCStatement -> Maybe HLCStatement
cleanBlockStmt (BlockStmt block)
  | null $ fromStatementList $ blockStmts block = Nothing
  | otherwise = Just $ BlockStmt block
cleanBlockStmt x = Just $ cleanEmptyBlocks x

cleanNoopJumps :: forall b. (Data b) => b -> b
cleanNoopJumps elt =
  case eqT of
    (Just (Refl :: StatementList :~: b)) ->
      StatementList $ cleanStmtNoops $ fromStatementList elt
    Nothing ->
      gmapT cleanNoopJumps elt

cleanStmtNoops :: [HLCStatement] -> [HLCStatement]
cleanStmtNoops (JumpStmt jmp :(LabelStmt lbl:xs))
  | lbl == jmp = cleanStmtNoops xs
  | otherwise = LabelStmt lbl : (JumpStmt jmp : cleanStmtNoops xs)
cleanStmtNoops (x:xs) = cleanNoopJumps x : cleanStmtNoops xs
cleanStmtNoops [] = []

cleanLbls :: forall b. (Data b) => S.Set HLCSymbol -> b -> Maybe b
cleanLbls symbolSet elt =
  case eqT of
    (Just (Refl :: HLCStatement :~: b)) ->
      case elt of
        (LabelStmt symb) ->
          case S.member symb symbolSet of
            True -> Just elt
            False -> Nothing
        _ -> gmapM (cleanLbls symbolSet) elt
    (Nothing) ->
      case eqT of
        (Just (Refl :: StatementList :~: b)) ->
          let stmts = fromStatementList elt in
          Just $ StatementList $ mapMaybe (cleanLbls symbolSet) stmts
        Nothing ->
          gmapM (cleanLbls symbolSet) elt

getJumps :: forall b. (Data b) => b -> [HLCSymbol]
getJumps elt =
  case eqT of
    (Just (Refl :: HLCStatement :~: b)) ->
      case elt of
        (JumpStmt symb) -> [symb]
        _ -> concat $ gmapQ getJumps elt
    (Nothing) ->
      concat $ gmapQ getJumps elt
