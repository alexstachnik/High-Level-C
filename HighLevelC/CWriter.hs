{-# LANGUAGE RecordWildCards #-}
module HighLevelC.CWriter where

import qualified Data.Sequence as Sq

import Data.Sequence((><),(|>))
import Data.Foldable(toList)

import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Pretty

import Text.PrettyPrint.HughesPJ

import HighLevelC.HLCTypes

data CWriter = CWriter {functionPrototypes :: Sq.Seq (CDeclaration NodeInfo),
                        code :: Sq.Seq CExtDecl}

instance Monoid CWriter where
  mempty = CWriter {functionPrototypes = Sq.empty,
                    code = Sq.empty}
  mappend a b = CWriter {functionPrototypes = functionPrototypes a >< functionPrototypes b,
                         code = code a >< code b}

instance Pretty CWriter where
  pretty (CWriter {..}) =
    (vcat $ map pretty $ toList functionPrototypes) $+$
    (vcat $ map pretty $ toList code)
