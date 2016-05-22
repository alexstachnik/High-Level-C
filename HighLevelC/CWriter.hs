{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module HighLevelC.CWriter where

import qualified Data.Sequence as Sq

import Data.Sequence((><),(|>))
import Data.Foldable(toList)

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Pretty

import Text.PrettyPrint.HughesPJ

import Control.Monad.Writer

import Util.Names
import IntermediateLang.ILTypes
import HighLevelC.HLCTypes


data CWriter = CWriter {functionProtos :: Sq.Seq FunctionProto,
                        structProtos :: Sq.Seq StructProto,
                        structDefs :: Sq.Seq StructDef,
                        funcDefs :: Sq.Seq FunctionDef,
                        stmts :: Sq.Seq HLCStatement,
                        varDecls :: Sq.Seq Variable}
             deriving (Show)

instance Monoid CWriter where
  mempty = CWriter {functionProtos = Sq.empty,
                    structProtos = Sq.empty,
                    structDefs = Sq.empty,
                    funcDefs = Sq.empty,
                    stmts = Sq.empty,
                    varDecls = Sq.empty}
  mappend a b = CWriter {functionProtos = functionProtos a >< functionProtos b,
                         structProtos = structProtos a >< structProtos b,
                         structDefs = structDefs a >< structDefs b,
                         funcDefs = funcDefs a >< funcDefs b,
                         stmts = stmts a >< stmts b,
                         varDecls = varDecls a >< varDecls b}

hideBlock :: CWriter -> CWriter
hideBlock (CWriter {..}) = CWriter {stmts=Sq.empty,
                                    varDecls = Sq.empty,
                                    ..}

grabBlock :: (MonadWriter CWriter m) => m a -> m HLCBlock
grabBlock m = censor hideBlock $ do
  (_,c) <- listen m
  return $ HLCBlock (toList $ varDecls c) (toList $ stmts c)
  

writeVar :: (MonadWriter CWriter m) => Variable ->  m ()
writeVar var = tell $ mempty {varDecls = Sq.singleton var}

writeStmt :: (MonadWriter CWriter m) => HLCStatement ->  m ()
writeStmt stmt = tell $ mempty {stmts = Sq.singleton stmt}

writeFunc :: (MonadWriter CWriter m) => FunctionDef ->  m ()
writeFunc func = tell $ mempty {funcDefs = Sq.singleton func}

writeStruct :: (MonadWriter CWriter m) => StructDef ->  m ()
writeStruct struct = tell $ mempty {structDefs = Sq.singleton struct}

writeStructProto :: (MonadWriter CWriter m) => StructProto ->  m ()
writeStructProto proto = tell $ mempty {structProtos = Sq.singleton proto}

writeFuncProto :: (MonadWriter CWriter m) => FunctionProto ->  m ()
writeFuncProto proto = tell $ mempty {functionProtos = Sq.singleton proto}
