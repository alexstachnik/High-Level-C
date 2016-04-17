{-# LANGUAGE TemplateHaskell #-}

module Util.THUtil where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

unpackList :: Int -> Name -> Q Exp
unpackList n fname =  do
  names <- mapM newName $ replicate n "x"
  lamE [listP $ map varP names] (appsE $ map varE (fname:names))

