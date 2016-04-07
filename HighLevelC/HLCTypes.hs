{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HighLevelC.HLCTypes where

import Language.C.Pretty(Pretty,pretty)
import Text.PrettyPrint.HughesPJ

import Data.List

import qualified Data.Map as M
import qualified Data.Set as S

import IntermediateLang.ILTypes

newtype Symbol = Symbol
                 {fromSymbol :: String}
               deriving (Eq,Ord,Show)

newtype SymbolTable = SymbolTable {fromSymbolTable :: S.Set Symbol}

data Variable = Variable Symbol ILType
              deriving (Eq,Ord,Show)

data Argument = Argument Symbol ILType
              deriving (Eq,Ord,Show)

data PartialFunDecl a = PartialFunDecl ILType a

emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable S.empty

addSymbolToTable :: Symbol -> SymbolTable -> (Bool,SymbolTable)
addSymbolToTable name (SymbolTable tbl) =
  (S.member name tbl,SymbolTable $ S.insert name tbl)

makeSymbol :: String -> Symbol
makeSymbol = Symbol . concat . filter notDoubleUnderscore . group
  where notDoubleUnderscore "__" = False
        notDoubleUnderscore _ = True

zEncodeType :: ILType -> String
zEncodeType = zEncode . show . pretty . writeDecl ""

zEncode :: String -> String
zEncode = concatMap zEncodeChar

zEncodeChar :: Char -> String
zEncodeChar ' ' = "zw"
zEncodeChar '_' = "zu"
zEncodeChar '(' = "ZL"
zEncodeChar ')' = "ZR"
zEncodeChar '*' = "zt"
zEncodeChar 'z' = "zz"
zEncodeChar 'Z' = "ZZ"
zEncodeChar x = [x]

sepSymbolElts :: [String] -> String
sepSymbolElts (x:y:xs) = x ++ "__" ++ sepSymbolElts (y:xs)
sepSymbolElts [x] = x
sepSymbolElts [] = ""

makeDerivedSymbol :: Symbol -> [ILType] -> Symbol
makeDerivedSymbol (Symbol name) args =
  Symbol $ sepSymbolElts (name : map zEncodeType args)

argToType :: Argument -> ILType
argToType (Argument _ x) = x

newtype TW a = TW {fromTW :: ILType}

class HLCTypeable a where
  hlcType :: TW a
  getObjType :: a -> ILType
  makeElt :: a

  getObjType _ = fromTW (hlcType :: TW a)

instance Pretty Symbol where
  pretty = text . fromSymbol

instance Pretty Argument where
  pretty (Argument name ty) = pretty $ writeDecl (fromSymbol name) ty

