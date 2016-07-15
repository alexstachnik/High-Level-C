{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Language.HLC.HighLevelC.PrimFunctions where

import Language.HLC.Util.Names

import Data.Typeable
import Language.HLC.HighLevelC.HLC
import Language.HLC.HighLevelC.HLCTypes
import Language.HLC.HighLevelC.BasicTypes
import Language.HLC.HighLevelC.CWriter
import Language.HLC.HighLevelC.Operators
import Language.HLC.HighLevelC.HLCCalls
import Language.HLC.HighLevelC.LangConstructs

malloc :: ExtFunction '[HLCInt] (HLCPtr HLCVoid) False
malloc = ExtFunction (ExactSymbol "malloc") [PreprocessorDirective "#include <stdlib.h>"]

call_malloc :: forall a intTy ptrTy.
               (HLCBasicIntType intTy,
                RHSExpression a intTy,
                HLCTypeable ptrTy) =>
               a -> HLC (TypedExpr (HLCPtr ptrTy))
call_malloc n =
  let n' = fromIntType $ rhsExpr n :: HLC (TypedExpr HLCInt) in
  castPtr $ callExtFunction malloc (n' :+: HNil)


freeMem :: ExtFunction '[HLCPtr a] HLCVoid False
freeMem = ExtFunction (ExactSymbol "free") [PreprocessorDirective "#include <stdlib.h>"]

call_freeMem :: forall a obj.
                (RHSExpression a (HLCPtr obj),
                 HLCTypeable obj) => a -> HLC (TypedExpr HLCVoid)
call_freeMem ptr =
  callExtFunction
  (freeMem :: ExtFunction '[HLCPtr obj] HLCVoid False)
  ((rhsExpr ptr) :+: HNil)

printf :: ExtFunction '[HLCString] HLCVoid True
printf = ExtFunction (ExactSymbol "printf") [PreprocessorDirective "#include <stdio.h>"]


