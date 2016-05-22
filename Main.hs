{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}

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
instance HLCTypeable a => HLCTypeable (MyStruct a) where
  hlcType = structHLCType

data FieldA deriving (Typeable)
data FieldB deriving (Typeable)

instance (HLCTypeable a) => StructFieldClass (MyStruct a) FieldA a
instance (HLCTypeable a) => StructFieldClass (MyStruct a) FieldB HLCChar

instance (HLCTypeable a) => Struct (MyStruct a) where
  structContents _ = [makeStructField (Proxy :: Proxy FieldA) Nothing,
                      makeStructField (Proxy :: Proxy FieldB) (Just 5)]




--q :: Function (TypedExpr HLCChar -> (HLC (TypedExpr HLCInt)))
--q = ExtFunc (ExtSymbol "foo")

q = [cExpr|sizeof(int)|]
