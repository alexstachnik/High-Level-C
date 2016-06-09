module Parser.Parser where

import AST.AST

import Data.Word
import Data.Maybe

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

TokenParser{parens = cParens,
            identifier = cIdent,
            reservedOp = cReservedOp,
            reserved = cReserved,
            semi = cSemi,
            semiSep1 = cSemiSep1,
            whiteSpace = cWhiteSpace,
            charLiteral = cCharLit,
            stringLiteral = cStringLit,
            naturalOrFloat = cNaturalOrFloat,
            braces = cBraces} =
  makeTokenParser (javaStyle {reservedNames = ["if"],
                              reservedOpNames = ["=","+","-","*","/","%","&&","||","&","|","^"],
                              identStart = upper})

exprParser :: Parsec String u Expr
exprParser = buildExpressionParser exprTbl term <?> "expression"

exprTbl = [[Infix (cReservedOp "=" >> return Assign) AssocLeft]]

numToLit :: Either Integer Double -> HLCLit
numToLit (Left n) = IntLit n
numToLit (Right f) = DoubleLit f

toWord8 :: Int -> Word8
toWord8 n
  | (n >= 0) && (n <= 255) = fromIntegral n
  | otherwise = error "Out of bounds"

term = cParens exprParser
  <|> (cIdent >>= (return . Ident))
  <|> (cNaturalOrFloat >>= (return . LitExpr . numToLit))
  <|> (cCharLit >>= return . LitExpr . CharLit . toWord8 . fromEnum)
  <|> (cStringLit >>= return . LitExpr . StrLit)

e str = case parse exprParser "" str of
  (Left err) -> print err
  (Right expr) -> print expr

exportParser :: Parsec String u [Export]
exportParser = do
  ret <- cBraces $ cSemiSep1 $ optionMaybe exportLine
  return $ catMaybes ret

exportLine :: Parsec String u Export
exportLine = do
  (fname:tyArgs) <- many1 cIdent
  return $ Export (DefiniteType fname) (map DefiniteType tyArgs)

tluParser :: Parsec String u TLU
tluParser = do
  cWhiteSpace
  exports <- exportParser
  eof
  return $ TLU exports []

parseFile :: String -> TLU
parseFile str = case parse tluParser "" str of
  Left err -> error $ show err
  Right tlu -> tlu
