module HighLevelC.Scope where

import qualified Data.Map as M

import IntermediateLang.ILTypes
import HighLevelC.HLCTypes

newtype Scope = Scope [M.Map Symbol ILType]
              deriving (Show,Eq,Ord)

emptyScope :: Scope
emptyScope = Scope [M.empty]

popScope :: Scope -> Scope
popScope (Scope (x:xs)) = Scope xs

pushScope :: Scope -> Scope
pushScope (Scope xs) = Scope (M.empty : xs)

lookupSymbol :: Scope -> Symbol -> Maybe ILType
lookupSymbol (Scope (x:xs)) symb =
  case M.lookup symb x of
    Just ty -> Just ty
    Nothing -> lookupSymbol (Scope xs) symb
lookupSymbol (Scope []) _ = Nothing

addToScope :: Scope -> Symbol -> ILType -> Scope
addToScope (Scope (x:xs)) symb ty = Scope (M.insert symb ty x:xs)

