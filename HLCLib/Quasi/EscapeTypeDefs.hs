module Quasi.EscapeTypeDefs(scanForTokens) where

import Data.Maybe(catMaybes)
import Text.Parsec(Parsec,string,manyTill,anyChar,space,(<|>),
                   eof,many,try,parse)

escapeSeq :: Parsec String u String
escapeSeq = do
  _ <- string "$$"
  str <- manyTill anyChar ((space >> return ()) <|> eof)
  return ("$$" ++ str)

tokenParser :: Parsec String u [String]
tokenParser = do
  toks <- many (fmap Just (try escapeSeq) <|> fmap (const Nothing) anyChar)
  return $ catMaybes toks

scanForTokens s = case parse tokenParser "" s of
  Left err -> error $ show err
  Right toks -> toks


