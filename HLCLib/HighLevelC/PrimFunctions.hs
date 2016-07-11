{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HighLevelC.PrimFunctions where

import Util.Names

import Data.Typeable
import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes
import HighLevelC.CWriter
import HighLevelC.Operators
import HighLevelC.HLCCalls
import HighLevelC.LangConstructs


malloc :: ExtFunction (HLCInt -> HLCPtr HLCVoid)
malloc = ExtFunction (ExactSymbol "malloc") [PreprocessorDirective "#include <stdlib.h>"]


freeMem :: ExtFunction (HLCPtr a -> HLCVoid)
freeMem = ExtFunction (ExactSymbol "free") [PreprocessorDirective "#include <stdlib.h>"]


printf :: ExtFunction (HLCString -> HLCVoid)
printf = VarExtFunction (ExactSymbol "printf") [PreprocessorDirective "#include <stdio.h>"]
