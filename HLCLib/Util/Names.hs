{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util.Names where

import Language.C.Data.Ident

import qualified Data.Map as M
import Data.List
import Data.Char
import Data.Data
import Data.String

newtype SafeName = SafeName {fromSafeName :: String}
                 deriving (Eq,Ord,Show,Data,Typeable,IsString)

data HLCSymbol = HLCSymbol {hlcSymbolUID :: Integer,
                            hlcSymbolPrefName :: SafeName}
               | ExactSymbol {exactSymbolName :: String}
            deriving (Eq,Ord,Show,Data,Typeable)

notDoubleUnderscore :: String -> Bool
notDoubleUnderscore = not . isPrefixOf "__"

makeSafeName :: String -> SafeName
makeSafeName =
  SafeName .
  concat . filter notDoubleUnderscore . group .
  filter (\c -> any ($c) [isAlphaNum, (==) '_']) . zEncode

zEncode :: String -> String
zEncode = concatMap zEncodeChar

zEncodeChar :: Char -> String
zEncodeChar ' ' = "_"
zEncodeChar '_' = "zu"
zEncodeChar 'z' = "zz"
zEncodeChar x = [x]

trim :: String -> String
trim str = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace str

joinSafeNames :: [SafeName] -> SafeName
joinSafeNames = SafeName . intercalate "__"  . map fromSafeName

extractExactSymbol :: HLCSymbol -> Ident
extractExactSymbol (ExactSymbol str) = internalIdent str
extractExactSymbol _ = error "Not an exact symbol"

safeNameToIdent :: SafeName -> Ident
safeNameToIdent = internalIdent . fromSafeName
