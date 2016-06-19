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
import HighLevelC.LangConstructs
import IntermediateLang.ILTypes
import PostProcess.Printer
import PostProcess.ObjectRewrite

import Language.Haskell.TH
import Language.Haskell.TH as TH


$(generateStructDesc [structDefn|PrimeField {order :: HLCInt} where
                                isPassable = True
                                constructor = primeFieldCons
                                destructor = primeFieldDest|])
primeFieldCons _ makeStructField cxt = do
  p <- makeStructField order
  p =: (intLit 2)
  return cxt
primeFieldDest _ cxt = return cxt

initPrimeField field n =
  field $. order =: n

class Group g elt | elt -> g where
  add' :: g -> elt -> elt -> elt
  toInt' :: elt -> Type_Int
add :: (ClassWrap2 Group g elt) =>
       FuncTyWrap3 g elt elt elt
add = funcWrap3 add'
toInt :: (ClassWrap2 Group g elt) =>
         FuncTyWrap1 elt HLCInt
toInt = funcWrap1 toInt'


$(generateStructDesc [structDefn|PrimeFieldElt {pfieldElt :: HLCInt} where|])


$(generateFunction [funcDefn|primeFieldAdd PrimeField -> PrimeFieldElt -> PrimeFieldElt -> PrimeFieldElt|])
primeFieldAdd ret field lhs rhs = do
  retVal <- makeLocalStruct type_PrimeFieldElt
  retVal $. pfieldElt =: (((lhs%.pfieldElt) %+ (rhs%.pfieldElt)) %% (field %. order))
  ret (lhsExpr retVal)

$(generateFunction [funcDefn|primeFieldToInt PrimeFieldElt -> HLCInt|])
primeFieldToInt ret elt = do
  ret $ (elt %. pfieldElt)

instance Group Type_PrimeField Type_PrimeFieldElt where
  add' = call_primeFieldAdd
  toInt' = call_primeFieldToInt

makePrimeFieldElt n = do
  elt <- makeLocalStruct type_PrimeFieldElt
  elt $. pfieldElt =: n
  return elt

$(generateFunction [funcDefn|arithmetic HLCInt|])
arithmetic ret = do
  field <- makeLocalStruct type_PrimeField
  initPrimeField field (intLit 7)
  m <- makePrimeFieldElt (intLit 5)
  n <- makePrimeFieldElt (intLit 2)
  m =: add field m n
  ifThenElse (toInt m %== intLit 0)
    (do _ <- callExt1 printf (stringLit "Success: 5+2=0 (mod 7)\n") NilArg
        ret (intLit 0))
    (do _ <- callExt1 printf (stringLit "Error\n") NilArg
        ret (intLit 1))



$(generateFunction [funcDefn|test HLCVoid|])
test ret = do
  n <- intLit 15
  _ <- callExt1 printf (stringLit "Hello, world! This is an integer: %d\n") (ConsArg n NilArg)
  ret void

$(generateFunction [funcDefn|fact HLCInt -> HLCInt|])

fact ret n = do
  ifThenElse (n %<= intLit 1)
    (ret n)
    (ret $ (n %* (call_fact (n %- intLit 1))))

$(generateFunction [funcDefn|hlcMain HLCInt -> HLCWeakPtr (HLCWeakPtr HLCChar) -> HLCInt|])
hlcMain ret argc argv = do
  v <- makeLocalStruct type_Int
  exprStmt $ call_test
  exprStmt $ call_arithmetic
  ret (call_fact v)


main :: IO ()
main = print $ printWholeTU (Just 'hlcMain) $ runOuterHLC $ do
  _ <- call_test
  _ <- call_fact (withType :: Type_Int)
  _ <- call_hlcMain (withType :: Type_Int) (withType :: MkWeakPtr (MkWeakPtr Type_Char))
  _ <- call_primeFieldAdd (withType :: Type_PrimeField) (withType :: Type_PrimeFieldElt) (withType :: Type_PrimeFieldElt)
  _ <- call_primeFieldToInt (withType :: Type_PrimeFieldElt)
  _ <- call_arithmetic
  return ()
