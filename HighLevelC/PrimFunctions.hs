module HighLevelC.PrimFunctions where

import Util.Names
import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes

malloc :: Function (TypedExpr HLCInt -> HLC (TypedExpr (HLCPointer HLCVoid)))
malloc = ExtFunc $ ExtSymbol "malloc"

