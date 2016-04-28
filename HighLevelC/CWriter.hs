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
                        varDecls :: Sq.Seq Variable,
                        objectManagement :: Sq.Seq ObjectManager}
             deriving (Show)

instance Monoid CWriter where
  mempty = CWriter {functionProtos = Sq.empty,
                    structProtos = Sq.empty,
                    structDefs = Sq.empty,
                    funcDefs = Sq.empty,
                    stmts = Sq.empty,
                    varDecls = Sq.empty,
                    objectManagement = Sq.empty}
  mappend a b = CWriter {functionProtos = functionProtos a >< functionProtos b,
                         structProtos = structProtos a >< structProtos b,
                         structDefs = structDefs a >< structDefs b,
                         funcDefs = funcDefs a >< funcDefs b,
                         stmts = stmts a >< stmts b,
                         varDecls = varDecls a >< varDecls b,
                         objectManagement = objectManagement a >< objectManagement b}

hideBlock :: CWriter -> CWriter
hideBlock (CWriter {..}) = CWriter {stmts=Sq.empty,
                                    objectManagement=Sq.empty,
                                    varDecls = Sq.empty,
                                    ..}

writeFunctionM :: forall m a. (MonadWriter CWriter m, HLCTypeable a) =>
                  HLCSymbol -> [Argument] -> m (TypedExpr a) -> m (TypedExpr a)
writeFunctionM name args m = censor hideBlock $ do
  (result,writer) <- listen m
  let retType = fromTW (hlcType :: TW a)
  writeFunc $ FunctionDef {functionRetType = retType,
                           functionName = name,
                           functionArguments = args,
                           functionLocalVars = toList $ varDecls writer,
                           functionStmts = toList $ stmts writer,
                           functionObjectManagers = toList $ objectManagement writer}
  return result

writeVar :: (MonadWriter CWriter m) => Variable ->  m ()
writeVar var = tell $ mempty {varDecls = Sq.singleton var}

writeStmt :: (MonadWriter CWriter m) => HLCStatement ->  m ()
writeStmt stmt = tell $ mempty {stmts = Sq.singleton stmt}

writeObjectManager :: (MonadWriter CWriter m) => ObjectManager -> m ()
writeObjectManager manager = tell $ mempty {objectManagement = Sq.singleton manager}

writeFunc :: (MonadWriter CWriter m) => FunctionDef ->  m ()
writeFunc func = tell $ mempty {funcDefs = Sq.singleton func}

writeStruct :: (MonadWriter CWriter m) => StructDef ->  m ()
writeStruct struct = tell $ mempty {structDefs = Sq.singleton struct}

writeStructProto :: (MonadWriter CWriter m) => StructProto ->  m ()
writeStructProto proto = tell $ mempty {structProtos = Sq.singleton proto}

writeFuncProto :: (MonadWriter CWriter m) => FunctionProto ->  m ()
writeFuncProto proto = tell $ mempty {functionProtos = Sq.singleton proto}
