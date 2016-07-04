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
import PostProcess.SymbolRewrite
import PostProcess.ObjectRewrite

import Language.Haskell.TH
import Language.Haskell.TH as TH




$(generateStructDesc [structDefn|PrimeField {order :: HLCInt} where
                                isPassable = True
                                constructor = primeFieldCons
                                destructor = primeFieldDest|])
primeFieldCons _ this cxt = do
  this %. order =: (intLit 2)
  return cxt
primeFieldDest _ cxt = return cxt

initPrimeField field n =
  field %. order =: n

class Group g elt | elt -> g where
  add' :: g -> elt -> elt -> elt
  toInt' :: elt -> Type_Int
add :: (ClassWrap2 Group g elt) =>
       FuncTyWrap3 g elt elt elt
add = funcWrap3 add'
toInt :: (ClassWrap2 Group g elt) =>
         FuncTyWrap1 elt HLCInt
toInt = funcWrap1 toInt'


$(generateStructDesc [structDefn|PrimeFieldElt {pfieldElt :: HLCInt}|])


$(generateFunction [funcDefn|primeFieldAdd PrimeField -> PrimeFieldElt -> PrimeFieldElt -> PrimeFieldElt|])
primeFieldAdd ret field lhs rhs = do
  retVal <- makeVar :: Var PrimeFieldElt
  retVal %. pfieldElt =: (((lhs%.pfieldElt) %+ (rhs%.pfieldElt)) %% (field %. order))
  ret (lhsExpr retVal)

$(generateFunction [funcDefn|primeFieldToInt PrimeFieldElt -> HLCInt|])
primeFieldToInt ret elt = do
  ret $ (elt %. pfieldElt)

instance Group (ExprTy PrimeField) (ExprTy PrimeFieldElt) where
  add' = call_primeFieldAdd
  toInt' = call_primeFieldToInt

makePrimeFieldElt n = do
  elt <- makeVar :: Var PrimeFieldElt
  elt %. pfieldElt =: n
  return elt

$(generateFunction [funcDefn|arithmetic HLCInt|])
arithmetic ret = do
  field <- makeVar :: Var PrimeField
  initPrimeField field (intLit 7)
  m <- makePrimeFieldElt (intLit 5)
  n <- makePrimeFieldElt (intLit 2)
  m =: add field m n
  ifThenElse (toInt m %== intLit 0)
    (do exprStmt $ callExt1 printf (stringLit "Success: 5+2=0 (mod 7)\n") NilArg
        ret (intLit 0))
    (do exprStmt $ callExt1 printf (stringLit "Error\n") NilArg
        ret (intLit 1))



$(generateFunction [funcDefn|test HLCVoid|])
test ret = do
  n <- intLit 15
  ifThenElseRest (intLit 15 %<= intLit 5)
    (\c -> do
        x <- makeVar :: Var HLCInt
        x =: intLit 12
        c
    )
    (\c -> do
        x <- makeVar :: Var HLCInt
        x =: intLit 13
        c
    )
  whileStmt (intLit 3 %<= intLit 10)
    (\break cont -> do
        x <- makeVar :: Var HLCInt
        x =: intLit 7
        ifThenElseRest (intLit 3 %<= intLit 10)
          (\c -> do
              break
          )
          id
        cont
    )
  exprStmt $ callExt1 printf (stringLit "Hello, world! This is an integer: %d\n") (ConsArg n NilArg)
  ret void

$(generateFunction [funcDefn|fact HLCInt -> HLCInt|])

fact :: (forall a. (RHSExpression a HLCInt) => a -> HLC Context) -> TypedLHS HLCInt -> HLC Context
fact ret n = do
  ifThenElse (n %<= intLit 1)
    (ret n)
    (ret $ (n %* (call_fact (n %- intLit 1))))

z = generateFunction [funcDefn|hlcMain HLCInt -> HLCPtr (HLCPtr HLCChar) -> HLCInt|]

$(generateFunction [funcDefn|hlcMain HLCInt -> HLCPtr (HLCPtr HLCChar) -> HLCInt|])
hlcMain ret argc argv = do
  v <- makeVar :: Var HLCInt
  argc =: v
  argv =: nullPtr
--  a <- allocMem type_Int (intLit 1)
--  b <- allocMem type_Int (intLit 2)
  exprStmt $ call_test
  exprStmt $ call_arithmetic
  ret (call_fact (intLit 5))


main :: IO ()
main = print $ printWholeTU (Just 'hlcMain) $ runOuterHLC $ do
--main = print $ processObjects $ processSymbols $ runOuterHLC $ do
  _ <- call_test
  _ <- call_fact withType
  _ <- call_hlcMain withType withType
  _ <- call_primeFieldAdd withType withType withType
  _ <- call_primeFieldToInt (withType :: (ExprTy PrimeFieldElt))
  _ <- call_arithmetic
  return ()
