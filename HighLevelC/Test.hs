{-# LANGUAGE TemplateHaskell #-}
module HighLevelC.Test where

import HighLevelC.Test2

f = $(q)
