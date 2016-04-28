module Util.Names where

import qualified Data.Map as M
import Data.List
import Data.Char

newtype SafeName = SafeName {fromSafeName :: String}
                 deriving (Eq,Ord,Show)

data HLCSymbol = HLCSymbol {hlcSymbolUID :: Integer,
                            hlcSymbolPrefName :: SafeName}
               | ExtSymbol {extSymbolName :: String}
            deriving (Eq,Ord,Show)

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
