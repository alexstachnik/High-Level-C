{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
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

module Main(main) where



import Language.C.Data.InputStream
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.Node
import Language.C.Data.Name
import Language.C.Parser
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants

import Debug.Trace
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
import IntermediateLang.ILTypes
import Printer.Printer
import Printer.PostProcess

import Language.Haskell.TH
import Language.Haskell.TH as TH

main :: IO ()
main = print 3





$(generateStructDesc yStruct)
someCons _ makeStructField cxt = do
  fieldA <- makeStructField (Proxy :: Proxy FieldA)
  fieldB <- makeStructField (Proxy :: Proxy FieldB)
  return cxt
someDest _ cxt = do
  return cxt


$(generateFunction xFunc)
hey ret n = ret (fromIntType n :: TypedExpr HLCChar)


x :: HLC ()
x = do
--  _ <- someFunc (undefined :: HLC (TypedExpr HLCInt))
  foo :: TypedVar HLCInt <- makePrimVar "foo"
  return ()

fff = do
  _ <- callHey (return undefined :: HLC (TypedExpr HLCInt))
  return ()

$(generateStructDesc [structDefn|SomeStructType forall a1 a2.  =>
                                {FieldAA :: a1,FieldBB :: a2,FieldCC :: HLCInt} where
                                isPassable = True
                                constructor = cons2
                                destructor = dest2|])

cons2 _ makeStructField cxt = do
  fieldA <- makeStructField (Proxy :: Proxy FieldAA)
  fieldB <- makeStructField (Proxy :: Proxy FieldBB)
  fieldC <- makeStructField (Proxy :: Proxy FieldCC)
  return cxt
dest2 _ cxt = do
  return cxt


$(generateFunction [funcDefn|SomeFunc someFunc callSomeFunc (HLCBasicIntType a1) => a1 -> HLCInt -> HLCChar|])
someFunc ret n m = ret (fromIntType m)
