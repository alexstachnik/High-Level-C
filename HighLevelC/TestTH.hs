{-# LANGUAGE TemplateHaskell #-}
module HighLevelC.TestTH where

import Language.Haskell.TH
import HighLevelC.Test2


$( makeFunction "test" (litE $ integerL 3))

$( g )
