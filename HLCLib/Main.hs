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

appsT :: [TypeQ] -> TypeQ
appsT [x] = x
appsT (x:y:xs) = appsT ((appT x y) : xs)

makePassableStruct :: Int -> TH.Name -> TH.Name -> Q Dec
makePassableStruct numParams consName destName = do
  vars <- mapM newName (replicate numParams "a")
  let typeableConstraint = map (\v -> appT [t|HLCTypeable|] (varT v)) vars
      passableConstraint = map
        (\v -> appT (appT equalityT (appT [t|Passability|] (varT v))) [t|IsPassable|])
        vars
      constraints = sequence (typeableConstraint ++ passableConstraint)
      classTy = appsT [[t|Struct|],[t|IsPassable|],appsT ([t|MyStruct|]:map varT vars)]
  makeStructField <- newName "makeStructField"
  cxt <- newName "cxt"
  let cons = funD (mkName "constructor")
       [clause [] (normalB $ varE consName) []]
      dest = funD (mkName "destructor")
        [clause [] (normalB $ varE destName) []]
  instanceD constraints classTy [cons,dest]

instance (HLCTypeable a,Passability a ~ IsPassable) => Struct IsPassable (MyStruct a) where
  constructor _ makeStructField cxt = do
    fieldA <- makeStructField (Proxy :: Proxy FieldA)
    fieldB <- makeStructField (Proxy :: Proxy FieldB)
    return cxt
  destructor _ cxt = return cxt


data SomeFunc a1
instance HLCFunction (SomeFunc HLCInt) (ArgWrap1 (TypedExpr HLCInt)) HLCChar where
  call _ ret = ArgWrap1 (\n -> (ret (fromIntType n :: TypedExpr HLCChar)))

x :: HLC ()
x = do
  _ <- call1 (Proxy :: Proxy (SomeFunc HLCInt)) undefined
  return ()
