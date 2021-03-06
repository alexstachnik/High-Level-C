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

module Test where

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

import Language.HLC.Util.Names
import Language.HLC.Quasi.Parser
import Language.HLC.Quasi.QuasiC
import Language.HLC.Quasi.QuasiTypes
import Language.HLC.HighLevelC.HLC
import Language.HLC.HighLevelC.HLCCalls
import Language.HLC.HighLevelC.HLCTypes
import Language.HLC.HighLevelC.CWriter
import Language.HLC.HighLevelC.BasicTypes
import Language.HLC.HighLevelC.PrimFunctions
import Language.HLC.HighLevelC.VarDecls
import Language.HLC.HighLevelC.Operators
import Language.HLC.HighLevelC.TypeSynonyms
import Language.HLC.HighLevelC.LangConstructs
import Language.HLC.Libraries.Memory
import Language.HLC.IntermediateLang.ILTypes
import Language.HLC.PostProcess.Printer
import Language.HLC.PostProcess.SymbolRewrite
import Language.HLC.PostProcess.ObjectRewrite

import Language.Haskell.TH
import Language.Haskell.TH as TH

$(generateStructDesc [structDefn|StructA {} where
                                isPassable = True
                                constructor = consA
                                destructor = destA|])
consA _ this ret = do
  v <- makeVar :: Var HLCInt
  v =: intLit 3
  ret

destA _ this ret = do
  v <- makeVar :: Var HLCInt
  v =: intLit 4
  ret


$(generateStructDesc [structDefn|StructB {structBElt :: StructA} where
                                isPassable = True
                                constructor = consB
                                destructor = destB|])
consB _ this ret = do
  v <- makeVar :: Var HLCChar
  v =: fromIntType (intLit 5)
  ret

destB _ this ret = do
  v <- makeVar :: Var HLCChar
  v =: fromIntType (intLit 6)
  ret


$(generateStructDesc [structDefn|PrimeField {order :: HLCInt,foo :: HLCPrimArray HLCChar 3} where
                                isPassable = True
                                constructor = primeFieldCons
                                destructor = primeFieldDest|])



primeFieldCons _ this ret = do
  this %. order =: (intLit 2)
  ret
primeFieldDest _ this ret = ret

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
  q <- makeVar :: Var (HLCPrimArray HLCInt 5)
  m <- makePrimeFieldElt (intLit 5)
  n <- makePrimeFieldElt (intLit 2)
  m =: add field m n
  ifThenElse (toInt m %== intLit 0)
    (do exprStmt $ callVarFunction printf (stringLit "Success: 5+2=0 (mod 7)\n" :+: HNil)
        ret (intLit 0))
    (do exprStmt $ callVarFunction printf (stringLit "Error\n" :+: HNil)
        ret (intLit 1))



$(generateFunction [funcDefn|test HLCVoid|])
test ret = do
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
  exprStmt $ callVarFunction printf
    (stringLit "Hello, world! This is an integer: %d\n" :+: intLit 15 :+: HNil)
  ret void

$(generateFunction [funcDefn|fact HLCInt -> HLCInt|])
fact ret n = do
  ifThenElse (n %<= intLit 1)
    (ret n)
    (ret $ (n %* (call_fact (n %- intLit 1))))

$(generateFunction [funcDefn|hlcMain HLCInt -> HLCPtr (HLCPtr HLCChar) -> HLCInt|])
hlcMain ret argc argv = do
  v <- makeVar :: Var HLCInt
  argc =: v
  argv =: nullPtr
  a <- makeUniquePtr (intLit 1) :: Var (UniquePtr HLCInt)
  b <- makeUniquePtr (intLit 2) :: Var (UniquePtr PrimeFieldElt)
  c <- makeVar :: Var (HLCPtr HLCInt)
  d <- makeUniquePtr (intLit 2) :: Var (UniquePtr HLCInt)
  deref c =: intLit 3
  deref (weakRef a) =: intLit 2
  (weakRef a %@ intLit 0) =: intLit 10
  (weakRef a %@ intLit 1) =: intLit 10
  exprStmt $ call_test
  exprStmt $ call_arithmetic
  v1 <- makeVar :: Var StructA
  v2 <- makeVar :: Var StructB
  v3 <- makeVar :: Var (FunctionPtr '[HLCInt] HLCInt)
  v3 =: addrOfFunc (Proxy :: Proxy Fact)
  exprStmt $ callFunPtr v3 (intLit 4 :+: HNil)
  ret (call_fact (intLit 5))



main :: IO ()
main = print $ printWholeTU (Just 'hlcMain) $ runOuterHLC $ do
  _ <- call_test
  _ <- call_fact withType
  _ <- call_hlcMain withType withType
  _ <- call_primeFieldAdd withType withType withType
  _ <- call_primeFieldToInt (withType :: (ExprTy PrimeFieldElt))
  declareObj (Proxy :: Proxy StructA)
  declareObj (Proxy :: Proxy StructB)
  _ <- call_arithmetic
  return ()
