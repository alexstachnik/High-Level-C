{-# LANGUAGE TemplateHaskell #-}

module Language.HLC.Util.THUtil where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Char

unpackList :: Int -> Name -> Q Exp
unpackList n fname =  do
  names <- mapM newName $ replicate n "x"
  lamE [listP $ map varP names] (appsE $ map varE (fname:names))

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize [] = []
