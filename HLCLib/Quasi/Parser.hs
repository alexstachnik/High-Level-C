
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Quasi.Parser where

import Quasi.QuasiTypes
import Quasi.TypeParser

import Language.Haskell.TH
import Data.Maybe

import Data.Typeable
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

import Language.Haskell.TH.Quote

y = "someFunc callSomeFunc (HLCBasicIntType a1) => a1 -> a2 -> HLCInt -> HLCChar"
yy = "a1 -> a2 -> HLCInt -> HLCChar"

x = "SomeStructType forall a1 a2. (HLCTypeable a1, Passability a2 ~ IsPassable) => {FieldA :: a1,FieldB :: a2,FieldC :: HLCInt} where\n  isPassable = True\n  constructor = someCons\n  destructor = someDest"

structDefn :: QuasiQuoter
structDefn = QuasiQuoter {quoteExp = quoteStruct,
                          quotePat = undefined,
                          quoteDec = undefined,
                          quoteType = undefined}

quoteStruct str =
  case parse structParser "" str of
    Left err -> fail $ show err
    Right struct -> dataToExpQ (const Nothing) struct

funcDefn :: QuasiQuoter
funcDefn = QuasiQuoter {quoteExp = quoteFunc,
                        quotePat = undefined,
                        quoteDec = undefined,
                        quoteType = undefined}

quoteFunc str =
  case parse funcParser "" str of
    Left err -> fail $ show err
    Right struct -> dataToExpQ (const Nothing) struct

funcParser :: Parsec String u Function
funcParser = do
  body <- fmap mkName identifier
  callName <- fmap mkName identifier
  funcTypeStr <- many anyChar
  let funcType = parseSomeType funcTypeStr
      tyParams = getTyParams funcType
      constraints = getConstraints funcType
      retType = getRetType funcType
      argTypes = init $ getFuncComponents funcType
  return $ Function tyParams constraints retType argTypes body callName 


lexer = P.makeTokenParser haskellDef
parens = P.parens lexer
braces = P.braces lexer
identifier = P.identifier lexer
symbol = P.symbol lexer
dot = P.dot lexer
operator = P.operator lexer
commaSep = P.commaSep lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer

forallParser :: Parsec String u [TyVarBndr]
forallParser = do
  symbol "forall"
  vars <- many identifier
  dot
  return $ map (PlainTV . mkName) vars

someTypeParser :: Parsec String u Type
someTypeParser = do
  someType <- many (identifier <|> operator <|> (reservedOp "~" >> return "~"))
  return $ extTypeToTHType $ parseSomeType $ unwords someType

fieldParser :: Parsec String u Field
fieldParser = do
  fieldName <- identifier
  reservedOp "::"
  fieldTy <- someTypeParser
  return $ Field (mkName fieldName) fieldTy

propertyParser :: Parsec String u (String,String)
propertyParser = do
  propName <- identifier
  reservedOp "="
  propVal <- identifier
  return (propName,propVal)

structParser :: Parsec String u StructDesc
structParser = do
  structName <- fmap mkName identifier
  tyVars <- fmap (maybe [] id) $ optionMaybe forallParser
  someTypes <- fmap (maybe [] id) $ optionMaybe (parens $ commaSep someTypeParser)
  reservedOp "=>"
  fields <- braces $ commaSep fieldParser
  reserved "where"
  props <- many propertyParser
  let isPassable = read $ maybe "False" id $ lookup "isPassable" props :: Bool
      consName = mkName $ fromJust $ lookup "constructor" props
      destName = mkName $ fromJust $ lookup "destructor" props
  return $ StructDesc structName tyVars someTypes fields isPassable consName destName

parseStruct :: String -> StructDesc
parseStruct str = case parse structParser "" str of
  Left err -> error $ show err
  Right struct -> struct
