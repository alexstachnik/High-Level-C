{-# LANGUAGE RecordWildCards #-}
module HighLevelC.Scope where

import qualified Data.Map as M

import Util.Names

import IntermediateLang.ILTypes
import HighLevelC.HLCTypes

data Scope = Scope {typeNS :: DuplicateTable,
                    varNS :: DuplicateTable}
           deriving (Show,Eq,Ord)

emptyScope :: Scope
emptyScope = Scope {typeNS = emptyDuplicateTable,
                    varNS = emptyDuplicateTable}

makeTypeSymbol :: String -> Scope -> (Scope,Symbol)
makeTypeSymbol baseName (Scope {..}) =
  let (newTbl,symb) = makeSymbol baseName typeNS in
  (Scope {typeNS = newTbl,..},symb)

insertTypeSymbol :: Symbol -> Scope -> Scope
insertTypeSymbol symb (Scope {..}) =
  let newTbl = 

makeVarSymbol :: String -> Scope -> (Scope,Symbol)
makeVarSymbol baseName (Scope {..}) =
  let (newTbl,symb) = makeSymbol baseName varNS in
  (Scope {varNS = newTbl,..},symb)
