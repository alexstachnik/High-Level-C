
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Quasi.Parser where

import Debug.Trace

import Quasi.QuasiTypes
import Quasi.TypeParser

import Language.Haskell.TH
import Data.Maybe
import Data.List

import Data.Typeable
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes
import HighLevelC.VarDecls


import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

import Language.Haskell.TH.Quote


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
  funcTypeStr <- many anyChar
  let funcType = parseSomeType funcTypeStr
      tyParams = nub $ getTyParams funcType
      constraints = getConstraints funcType
      retType = getRetType funcType
      argTypes = init $ getFuncComponents funcType
  return $ Function tyParams constraints retType argTypes body 


lexer = P.makeTokenParser haskellDef
parens = P.parens lexer
braces = P.braces lexer
identifier = P.identifier lexer
symbol = P.symbol lexer
dot = P.dot lexer
comma = P.comma lexer
operator = P.operator lexer
commaSep = P.commaSep lexer
reservedOp = P.reservedOp lexer
reserved = P.reserved lexer

forallParser :: Parsec String u [TyVarBndr]
forallParser = do
  _ <- symbol "forall"
  vars <- many identifier
  _ <- dot
  return $ map (PlainTV . mkName) vars


betweenParens :: Parsec String u String
betweenParens = do
  choice [parens betweenParens,
          satisfy (/= ')') >>= (\x -> return [x])]
    
someTypeParser :: Parsec String u Type
someTypeParser = do
  someType <- many (choice [satisfy (\x -> (x /= ',') && (x /= '}')) >>=
                            (\x -> return [x]),
                            parens betweenParens])
  return $ extTypeToTHType $ parseSomeType $ concat someType

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

structTypesParser :: Parsec String u [Type]
structTypesParser = do
  tys <- parens $ commaSep someTypeParser
  reservedOp "=>"
  return tys

propertiesParser :: Parsec String u [(String,String)]
propertiesParser = do
  reserved "where"
  many propertyParser

structParser :: Parsec String u StructDesc
structParser = do
  structName <- fmap mkName identifier
  tyVars <- fmap (maybe [] id) $ optionMaybe forallParser
  someTypes <- fmap (maybe [] id) $ optionMaybe structTypesParser
  fields <- braces $ commaSep fieldParser
  props <- fmap (maybe [] id) $ optionMaybe propertiesParser
  let isPassable = read $ maybe "True" id $ lookup "isPassable" props :: Bool
      consName = maybe 'nullConstructor (mkName) $ lookup "constructor" props
      destName = maybe 'nullDestructor (mkName) $ lookup "destructor" props
      simplyInstanciable = fmap mkName $ lookup "Simple" props :: Maybe Name
  return $ StructDesc structName tyVars someTypes fields isPassable consName destName simplyInstanciable

parseStruct :: String -> StructDesc
parseStruct str = case parse structParser "" str of
  Left err -> error $ show err
  Right struct -> struct
