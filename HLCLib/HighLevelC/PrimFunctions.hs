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


allocMem :: forall a b b'. (HLCTypeable a,
                            Instanciable a (IsPrimitive a),
                            RHSExpression b b',
                            HLCBasicIntType b',HLCNumType b') =>
            b ->
            HLC (TypedLHS (HLCPtr a))
allocMem numElts = HLC $ do
  symb <- makeHLCSymbol_ "ptr"
  let numBytes = fromIntType $ hlcMul (hlcSizeof (Proxy :: Proxy a)) (rhsExpr numElts)
  consCont <- makeHLCSymbol_ $ makeSafeName "conscont"
  destCont <- makeHLCSymbol_ $ makeSafeName "destcont"
  writePreproDir (PreprocessorDirective "#include <stdint.h>")
  _ <- innerHLC $ declareObj (Proxy :: Proxy a)
  let ty = fromTW (hlcType :: TW (HLCPtr a))
      this = TypedVar symb
      retArg = TypedLHSVar this :: TypedLHS (HLCPtr a)
      consBody :: HLC_ Context
      consBody = do
        newPtr <- innerHLC $ callExt1 malloc numBytes NilArg
        writeStmt $ AssignmentStmt (LHSVar symb) $ fromTypedExpr newPtr
        return $ SomeContext consCont
      destBody :: HLC_ Context
      destBody = do
        innerHLC $ exprStmt $ callExt1 freeMem retArg NilArg
        return $ SomeContext destCont

  cons <- grabStructBlock consBody
  dest <- grabStructBlock destBody
  writeVar $ Variable symb ty Nothing cons dest
  return retArg


malloc :: ExtFunction (HLCInt -> HLCPtr HLCVoid)
malloc = ExtFunction (ExactSymbol "malloc") [PreprocessorDirective "#include <stdlib.h>"]


freeMem :: ExtFunction (HLCPtr a -> HLCVoid)
freeMem = ExtFunction (ExactSymbol "free") [PreprocessorDirective "#include <stdlib.h>"]


printf :: ExtFunction (HLCString -> HLCVoid)
printf = VarExtFunction (ExactSymbol "printf") [PreprocessorDirective "#include <stdio.h>"]
