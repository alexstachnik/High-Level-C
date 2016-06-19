{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module HighLevelC.CWriter where

import qualified Data.Sequence as Sq
import qualified Data.Set as S

import Debug.Trace

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


hideStructVarDecls :: CWriter -> CWriter
hideStructVarDecls (CWriter {..}) = CWriter {structVarDecls = Sq.empty,
                                             ..}

grabStructVars :: (MonadWriter CWriter m) => m a -> m [StructField]
grabStructVars m = censor (hideBlock . hideStructVarDecls) $ do
  (_,c) <- listen m
  return $ toList $ structVarDecls c

hideBlock :: CWriter -> CWriter
hideBlock (CWriter {..}) = CWriter {stmts=Sq.empty,
                                    varDecls = Sq.empty,
                                    ..}

grabStructBlock :: (MonadWriter CWriter m) => m Context -> m HLCBlock
grabStructBlock = censor hideStructVarDecls . grabBlock

grabBlock :: (MonadWriter CWriter m) => m Context -> m HLCBlock
grabBlock m = censor hideBlock $ do
  (cont,body) <- listen m
  return $ HLCBlock (toList $ varDecls body) (StatementList $ toList $ stmts body) cont
  

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

writeStructVarDecl :: (MonadWriter CWriter m) => StructField -> m ()
writeStructVarDecl field = tell $ mempty {structVarDecls = Sq.singleton field}

writePreproDir :: (MonadWriter CWriter m) => PreprocessorDirective -> m ()
writePreproDir dir = tell $ mempty {preproDirs = S.singleton dir}
