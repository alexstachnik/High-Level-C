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


module Libraries.Memory where

import Data.Proxy
import Data.Typeable


import Util.Names
import Quasi.Parser
import Quasi.QuasiC
import Quasi.QuasiTypes
import HighLevelC.HLC
import HighLevelC.HLCCalls
import HighLevelC.HLCTypes
import HighLevelC.CWriter
import HighLevelC.BasicTypes
import HighLevelC.PrimFunctions
import HighLevelC.VarDecls
import HighLevelC.Operators
import HighLevelC.TypeSynonyms
import HighLevelC.LangConstructs
import IntermediateLang.ILTypes
import PostProcess.Printer
import PostProcess.SymbolRewrite
import PostProcess.ObjectRewrite



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
  let numBytes = fromIntType $ hlcMul (hlcSizeof (Proxy :: Proxy b)) (rhsExpr n)
  (var %. uniquePtrElt) =: castPtr $ callExt1 malloc numBytes NilArg
  return var

uniqueArrDest _ this ret = do
  exprStmt $ callExt1 freeMem (this %. uniquePtrElt) NilArg
  ret

weakRef :: (LHSExpression a (UniquePtr a'),
            Instanciable a' (IsPrimitive a')) => a -> HLC (TypedLHS (HLCPtr a'))
weakRef ptr = do
  (hlcLHSExpr ptr) %. uniquePtrElt

