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


