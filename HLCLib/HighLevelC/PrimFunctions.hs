module HighLevelC.PrimFunctions where

import Util.Names
import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes
import HighLevelC.CWriter

callMalloc :: (HLCBasicIntType b) => TypedExpr b -> HLC_ HLCExpr
callMalloc len = do
  writePreproDirs (PreprocessorDirective "#include <stdlib.h>")
  return $ FunctionCall (LHSExpr $ LHSVar $ fromExtFunction malloc) [fromTypedExpr len]

malloc :: ExtFunction (HLCInt -> HLCPtr b HLCVoid)
malloc = ExtFunction $ ExactSymbol "malloc"

callFree :: TypedExpr (HLCPtr p a) -> HLC_ HLCExpr
callFree ptr = do
  writePreproDirs (PreprocessorDirective "#include <stdlib.h>")
  return $ FunctionCall (LHSExpr $ LHSVar $ fromExtFunction freeMem) [fromTypedExpr ptr]

freeMem :: ExtFunction (HLCPtr b a -> HLCVoid)
freeMem = ExtFunction $ ExactSymbol "free"

callPrintf :: TypedExpr HLCString -> VarArg -> HLC ()
callPrintf fmtStr args = HLC $ do
  writePreproDirs (PreprocessorDirective "#include <stdio.h>")
  writeStmt $ ExpStmt $ FunctionCall
    (LHSExpr $ LHSVar $ fromVarExtFunction printf)
    (fromTypedExpr fmtStr : varArgToList args)

printf :: ExtFunction (HLCString -> HLCVoid)
printf = VarExtFunction $ ExactSymbol "printf"
