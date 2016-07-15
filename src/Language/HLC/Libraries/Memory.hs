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


module Language.HLC.Libraries.Memory where

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



$(generateStructDesc [structDefn|UniquePtr forall a. {uniquePtrElt :: HLCPtr a} where
                                isPassable = False
                                destructor = uniqueArrDest |])

makeUniquePtr :: forall a a' b.
                 (RHSExpression a a',
                  HLCBasicIntType a',
                  HLCNumType a',
                 Instanciable b (IsPrimitive b)) =>
                 a -> Var (UniquePtr b)
makeUniquePtr n = do
  declareObj (Proxy :: Proxy b)
  var <- makeVar UniquePtr :: Var (UniquePtr b)
  let numBytes = hlcMul (hlcSizeof (Proxy :: Proxy b)) (rhsExpr n)
  (var %. uniquePtrElt) =: call_malloc numBytes
  return var

uniqueArrDest :: (HLCTypeable a, Instanciable a (IsPrimitive a)) =>
                 Proxy (UniquePtr a) ->
                 HLC (TypedLHS (UniquePtr a)) ->
                 HLC Context ->
                 HLC Context
uniqueArrDest _ this ret = do
  exprStmt $ call_freeMem (this %. uniquePtrElt)
  ret

weakRef :: (LHSExpression a (UniquePtr a'),
            Instanciable a' (IsPrimitive a')) => a -> HLC (TypedLHS (HLCPtr a'))
weakRef ptr = do
  (hlcLHSExpr ptr) %. uniquePtrElt

