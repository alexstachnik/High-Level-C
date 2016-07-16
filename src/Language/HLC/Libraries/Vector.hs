{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwarn-unused-do-bind #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Language.HLC.Libraries.Vector where

import Data.Proxy
import Data.Typeable


import Language.HLC.Util.Names
import Language.HLC.Quasi.Parser
import Language.HLC.Quasi.QuasiC
import Language.HLC.Quasi.QuasiTypes
import Language.HLC.HighLevelC.HLC
import Language.HLC.HighLevelC.HLCCalls
import Language.HLC.HighLevelC.HLCTypes
import Language.HLC.HighLevelC.CWriter
import Language.HLC.HighLevelC.BasicTypes
import Language.HLC.HighLevelC.PrimFunctions
import Language.HLC.HighLevelC.VarDecls
import Language.HLC.HighLevelC.Operators
import Language.HLC.HighLevelC.TypeSynonyms
import Language.HLC.HighLevelC.LangConstructs
import Language.HLC.IntermediateLang.ILTypes
import Language.HLC.PostProcess.Printer
import Language.HLC.PostProcess.SymbolRewrite
import Language.HLC.PostProcess.ObjectRewrite
import Language.HLC.Libraries.Memory

import Language.Haskell.TH

$(generateStructDesc [structDefn|Vector forall a. {
                                vectorArr :: UniquePtr a,
                                vectorLen :: HLCInt,
                                vectorArrSize :: HLCInt} where
                                isPassable = False
                                constructor = vectorCons
                                destructor = vectorDest |])

vectorCons _ this ret = do
  this %. vectorLen =: intLit 0
  this %. vectorArrSize =: intLit 2
  ret



vectorDest _ this ret = do
  ret

$(generateFunction [funcDefn|setVectorSize HLCPtr (Vector a) -> HLCInt ->  HLCVoid|])

setVectorSize ret vecPtr n = do
  moveUniquePtr (deref vecPtr %. vectorArr) (makeUniquePtr n)
  (deref vecPtr %. vectorArrSize) =: n
  (deref vecPtr %. vectorLen) =: n
  ret void
