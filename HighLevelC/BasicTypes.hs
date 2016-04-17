{-# LANGUAGE EmptyDataDecls #-}

module HighLevelC.BasicTypes where

import IntermediateLang.ILTypes

import HighLevelC.HLCTypes
import HighLevelC.HLC

import Util.Names

data HLCInt

instance HLCTypeable HLCInt where
  hlcType = TW (BaseType Signed NotConst ILInt)
  structDef = Nothing

makeInt :: String -> HLC (TypedVar HLCInt)
makeInt = makeVar . makeSafeName


f = do
  test <- makeInt "test"
  return ()
