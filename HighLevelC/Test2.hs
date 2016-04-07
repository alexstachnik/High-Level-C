{-# LANGUAGE TemplateHaskell #-}

module HighLevelC.Test2 where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
data MyObj = MyObj Int deriving (Show)

q = do
  putQ (MyObj 17)
  x <- getQ :: Q (Maybe MyObj)
  runIO $ print x
  [e|3|]
  

