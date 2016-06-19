module HighLevelC.PrimFunctions where

import Util.Names
import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes
import HighLevelC.CWriter

malloc :: ExtFunction (HLCInt -> HLCPtr b HLCVoid)
malloc = ExtFunction (ExactSymbol "malloc") [PreprocessorDirective "#include <stdlib.h>"]


freeMem :: ExtFunction (HLCPtr b a -> HLCVoid)
freeMem = ExtFunction (ExactSymbol "free") [PreprocessorDirective "#include <stdlib.h>"]


printf :: ExtFunction (HLCString -> HLCVoid)
printf = VarExtFunction (ExactSymbol "printf") [PreprocessorDirective "#include <stdio.h>"]
