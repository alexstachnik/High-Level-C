{-# LANGUAGE FunctionalDependencies #-}
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
import HighLevelC.Operators
import IntermediateLang.ILTypes
import Printer.Printer
import Printer.PostProcess

import Language.Haskell.TH
import Language.Haskell.TH as TH


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

class Group s elt | elt -> s where
  add :: s -> elt -> elt -> elt

$(generateStructDesc [structDefn|GaloisField => {Order :: HLCInt} where
                                isPassable = True
                                constructor = galoisCons
                                destructor = galoisDest|])
galoisCons _ makeStructField cxt = do
  order <- makeStructField (Proxy :: Proxy Order)
  assignVar order (intLit 7)
  return cxt
galoisDest _ cxt = return cxt

instance Group (HLC (TypedExpr GaloisField)) (HLC (TypedExpr HLCInt)) where
  add field lhs rhs = do
    field' <- field
    hlcMod (hlcAdd lhs rhs) (return $ readElt field' (Proxy :: Proxy Order))

$(generateFunction [funcDefn|doStuff callDoStuff HLCInt -> HLCInt|])

doStuff :: (TypedExpr HLCInt -> HLC b) -> TypedExpr HLCInt -> HLC b
doStuff ret n = do
  galois :: TypedVar GaloisField <- makeLocalStruct "GF"
  m :: TypedVar HLCInt <- makePrimVar "m"
  result <- add (return $ lhsExpr $ TypedLHSVar galois) (return n) (return $ lhsExpr $ TypedLHSVar m)
  ret result

$(generateFunction [funcDefn|someFunc callSomeFunc (HLCBasicIntType a1) => a1 -> HLCInt -> HLCChar|])
someFunc ret n m = do
  x :: TypedVar (HLCUniquePtr (SomeStructType HLCInt HLCInt)) <- allocMem "x" (intLit 3)
  assignVar (TypedLHSElement (TypedLHSDeref (TypedLHSVar x)) (Proxy :: Proxy FieldAA)) (intLit 5)
  n <- intLit 1
  assignVar (TypedLHSElement (TypedLHSDerefPlusOffset (TypedLHSVar x) n) (Proxy :: Proxy FieldAA)) (intLit 4)
  ret (fromIntType m)


fff = do
  _ <- callSomeFunc (return undefined :: HLC (TypedExpr HLCInt)) (return undefined)
  _ <- callDoStuff (return undefined :: HLC (TypedExpr HLCInt))
  return ()

main :: IO ()
main = print $ pretty $ printCWriter $ processSymbols $ runOuterHLC fff
