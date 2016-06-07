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

import Data.Typeable

import Util.Names
import Quasi.QuasiC
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


data MyStruct a
deriving instance Typeable a => Typeable (MyStruct a)
instance (HLCTypeable a, Passability a ~ IsPassable) => HLCTypeable (MyStruct a) where
  hlcType = structHLCType

data FieldA deriving (Typeable)
data FieldB deriving (Typeable)

instance (HLCTypeable a,Passability a ~ IsPassable) =>
         StructFieldClass IsPassable (MyStruct a) FieldA a
instance (HLCTypeable a,Passability a ~ IsPassable) =>
         StructFieldClass IsPassable (MyStruct a) FieldB HLCChar


instance (HLCTypeable a,Passability a ~ IsPassable) => Struct IsPassable (MyStruct a) where
  constructor _ makeStructField cxt = do
    fieldA <- makeStructField (Proxy :: Proxy FieldA)
    fieldB <- makeStructField (Proxy :: Proxy FieldB)
    return cxt
  destructor _ cxt = return cxt


data SomeFunc a1
instance (HLCTypeable a1,HLCBasicIntType a1) => HLCFunction (SomeFunc a1) (ArgWrap1 (TypedExpr a1)) HLCChar where
  call _ ret = ArgWrap1 (\n -> (ret (fromIntType n :: TypedExpr HLCChar)))

x :: HLC ()
x = do
  _ <- call1 (Proxy :: Proxy (SomeFunc HLCInt)) undefined
  return ()
