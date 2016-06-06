{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Printer.PostProcess where

import Data.Data
import Data.Typeable

import qualified Data.Map as M
import qualified Data.Set as S

import HighLevelC.HLCTypes
import Util.Names

processSymbols :: CWriter -> CWriter
processSymbols c =
  case fillInSymbols (S.fromList $ concat $ getExactSymbols c) c of
    (NameContext _ x) -> x

symbToString :: Bool -> HLCSymbol -> String
symbToString True (HLCSymbol uid prefName) = fromSafeName prefName ++ show uid
symbToString False (HLCSymbol uid prefName) = fromSafeName prefName
symbToString _ (ExactSymbol name) = name

addSymb :: HLCSymbol -> S.Set String -> S.Set String
addSymb (ExactSymbol _) set = set
addSymb symb@(HLCSymbol _ _) set = S.insert newName set
  where baseName = symbToString False symb
        newName = symbToString (S.member baseName set) symb

makeExact :: HLCSymbol -> S.Set String -> HLCSymbol
makeExact symb@(ExactSymbol _) _ = symb
makeExact symb@(HLCSymbol _ _) set = ExactSymbol newName
  where baseName = symbToString False symb
        newName = symbToString (S.member baseName set) symb

fillInSymbols :: (Data a) => S.Set String -> a -> NameContext a
fillInSymbols cxt d =
  gfoldl fillInHelper (NameContext cxt) d

fillInHelper :: forall d b. Data d => NameContext (d -> b) -> d -> NameContext b
fillInHelper (NameContext cxt cons) elt =
  case eqT of
    (Just (Refl :: HLCSymbol :~: d)) ->
     NameContext (addSymb elt cxt) (cons $ makeExact elt cxt)
    Nothing -> case eqT of
      (Just (Refl :: StatementList :~: d)) ->
        let (NameContext _ newElt) = fillInSymbols cxt elt in
        NameContext cxt (cons newElt)
      Nothing ->
        let (NameContext newCxt newElt) = fillInSymbols cxt elt in
        NameContext newCxt (cons newElt)
     

data NameContext a = NameContext (S.Set String) a

getExactSymbols :: (Data a) => a -> [[String]]
getExactSymbols = gmapQ getExactSymbolHelper

getExactSymbolHelper :: Data d => d -> [String]
getExactSymbolHelper d = case cast d of
          (Just (ExactSymbol str)) -> [str]
          (Just _) -> []
          Nothing -> concat $ getExactSymbols d
