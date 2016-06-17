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

module Main where

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
import HighLevelC.TypeSynonyms
import IntermediateLang.ILTypes
import Printer.Printer
import Printer.PostProcess

import Language.Haskell.TH
import Language.Haskell.TH as TH


$(generateFunction [funcDefn|testBinOp (HLCNumType a1, HLCBasicIntType a1) => a1 -> a1 -> HLCInt|])

testBinOp ret a b = do
  c <- makePrimVar type_Int
  c =: fromIntType (a %+ b)
  c =: c %+ intLit 10
  ret (lhsExpr c)

$(generateStructDesc [structDefn|SomeStructType forall a1.
                                {fieldAA :: a1,fieldBB :: a1,fieldCC :: HLCInt} where
                                isPassable = True
                                constructor = cons2
                                destructor = dest2|])

cons2 _ makeStructField cxt = do
  fieldA <- makeStructField fieldAA
  fieldB <- makeStructField fieldBB
  fieldC <- makeStructField fieldCC
  return cxt
dest2 _ cxt = do
  return cxt

class Group s elt | elt -> s where
  add' :: s -> elt -> elt -> elt

add :: (ClassWrap2 Group s elt) =>
       FuncTyWrap3 s elt elt elt
add = funcWrap3 add'

$(generateStructDesc [structDefn|GaloisField {order :: HLCInt} where
                                isPassable = True
                                constructor = galoisCons
                                destructor = galoisDest|])
galoisCons _ makeStructField cxt = do
  q <- makeStructField order
  q =: (intLit 7)
  return cxt
galoisDest _ cxt = return cxt

instance Group Type_GaloisField Type_Int where
  add' field lhs rhs = (lhs %+ rhs) %% (field %. order)

$(generateFunction [funcDefn|doStuff HLCInt -> HLCInt|])

doStuff :: (HLC (TypedExpr HLCInt) -> HLC b) -> HLC (TypedExpr HLCInt) -> HLC b
doStuff ret n = do
  galois <- makeLocalStruct type_GaloisField
  m <- makePrimVar type_Int
  m =: intLit 3
  m =: m %+ intLit 2
  _ <- add galois n n
  result <- add galois n m
  ret (return result)

$(generateFunction [funcDefn|someFunc (HLCBasicIntType a1) => a1 -> HLCInt -> HLCChar|])
someFunc ret n m = do
  x <- allocMem (type_SomeStructType type_Int) (intLit 3)
  lderef x $. fieldAA =: intLit 5
  n' <- intLit 1
  temp <- makePrimVar type_Int
  temp =: intLit 17
  exprStmt $ call_doStuff temp
  x $@ n' $. fieldAA =: intLit 4
  ret $ fromIntType m

$(generateFunction [funcDefn|pqp (HLCBasicIntType a1) => a1 -> HLCInt -> HLCChar|])
pqp ret n m = do
  x <- allocMem (type_SomeStructType type_Int) (intLit 3)
  lderef x $. fieldAA =: intLit 5
  n' <- intLit 1
  temp <- makePrimVar type_Int
  temp =: intLit 17
  exprStmt $ call_doStuff temp
  x $@ n' $. fieldAA =: intLit 4
  ret $ fromIntType m


type HLCIntT = HLC (TypedExpr HLCInt)

fff = do
  _ <- call_someFunc (withType :: Type_Int) (withType :: Type_Int)
  _ <- call_doStuff (withType :: Type_Int)
  _ <- call_testBinOp (withType :: Type_Char) (withType :: Type_Char)
  _ <- call_pqp (withType :: Type_Int) (withType :: Type_Int)
  return ()

main :: IO ()
main = print $ pretty $ printCWriter $ processSymbols $ runOuterHLC fff

