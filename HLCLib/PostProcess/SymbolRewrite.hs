{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module PostProcess.SymbolRewrite where

import Data.Data
import Data.Typeable

import qualified Data.Map as M
import qualified Data.Set as S

import HighLevelC.HLCTypes
import Util.Names

processSymbols :: CWriter -> CWriter
processSymbols c =
  case fillInSymbols (M.empty) (S.fromList $ concat $ getExactSymbols c) c of
    (NameContext _ _ x) -> x

makeExact :: HLCSymbol -> M.Map Integer String -> S.Set String -> NameContext HLCSymbol
makeExact symb@(ExactSymbol _) idMap set = NameContext idMap set symb
makeExact symb@(HLCSymbol uid prefName) idMap set =
  case M.lookup uid idMap of
    (Just name) -> NameContext idMap set $ ExactSymbol name
    Nothing -> let newName = case S.member (fromSafeName prefName) set of
                     True -> fromSafeName prefName ++ show uid
                     False -> fromSafeName prefName in
               NameContext (M.insert uid newName idMap) (S.insert newName set) $
               ExactSymbol newName

fillInSymbols :: (Data a) => M.Map Integer String -> S.Set String -> a -> NameContext a
fillInSymbols idMap cxt d =
  gfoldl fillInHelper (NameContext idMap cxt) d

fillInHelper :: forall d b. Data d => NameContext (d -> b) -> d -> NameContext b
fillInHelper (NameContext idMap cxt cons) elt =
  case eqT of
    (Just (Refl :: HLCSymbol :~: d)) ->
      let (NameContext newIdMap newCxt newElt) = makeExact elt idMap cxt in
      NameContext newIdMap newCxt (cons newElt)
    Nothing -> case eqT of
      (Just (Refl :: HLCBlock :~: d)) ->
        let (NameContext _ _ newElt) = fillInSymbols idMap cxt elt in
        NameContext idMap cxt (cons newElt)
      Nothing ->
        let (NameContext newIdMap newCxt newElt) = fillInSymbols idMap cxt elt in
        NameContext newIdMap newCxt (cons newElt)
     

data NameContext a = NameContext (M.Map Integer String) (S.Set String) a

getExactSymbols :: (Data a) => a -> [[String]]
getExactSymbols = gmapQ getExactSymbolHelper

getExactSymbolHelper :: Data d => d -> [String]
getExactSymbolHelper d = case cast d of
          (Just (ExactSymbol str)) -> [str]
          (Just _) -> []
          Nothing -> concat $ getExactSymbols d
