module HighLevelC.PrimFunctions where

import Util.Names
import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes

malloc :: ExtFunction (HLCInt -> HLCPtr b HLCVoid)
malloc = ExtFunction $ ExactSymbol "malloc"

freeMem :: ExtFunction (HLCPtr b a -> HLCVoid)
freeMem = ExtFunction $ ExactSymbol "free"
