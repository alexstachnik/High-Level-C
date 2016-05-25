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
import IntermediateLang.ILTypes

main :: IO ()
main = print 3


data MyStruct a
deriving instance Typeable a => Typeable (MyStruct a)
instance (HLCTypeable a, Passability a ~ IsPassable) => HLCTypeable (MyStruct a) where
  hlcType = structHLCType

data FieldA deriving (Typeable)
data FieldB deriving (Typeable)

instance (HLCTypeable a,Passability a ~ IsPassable) =>
         StructFieldClass IsPassable (MyStruct a) FieldA a 1
instance (HLCTypeable a,Passability a ~ IsPassable) =>
         StructFieldClass IsPassable (MyStruct a) FieldB HLCChar 1

instance (HLCTypeable a,Passability a ~ IsPassable) => Struct IsPassable (MyStruct a) where
  constructor _ makeStructField = do
    fieldA <- makeStructField (Proxy :: Proxy FieldA)
    fieldB <- makeStructField (Proxy :: Proxy FieldB)
    return ()
  destructor _ = return ()

data SomeFunc a1
instance HLCFunction (SomeFunc HLCInt) (ArgWrap1 (TypedExpr HLCInt)) HLCChar where
  call _ ret = ArgWrap1 (\n -> (ret (fromIntType n :: TypedExpr HLCChar)))




--q :: Function (TypedExpr HLCChar -> (HLC (TypedExpr HLCInt)))
--q = ExtFunc (ExtSymbol "foo")

q = [cExpr|sizeof(int)|]
