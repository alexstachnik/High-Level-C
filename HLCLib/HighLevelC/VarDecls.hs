{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module HighLevelC.VarDecls where

import Data.Typeable

import Util.Names

import HighLevelC.HLCTypes
import HighLevelC.HLC
import HighLevelC.BasicTypes
import HighLevelC.CWriter
import HighLevelC.PrimFunctions
import HighLevelC.Operators

import Language.Haskell.TH

makeWeakRef :: (RHSExpression c (HLCPtr p a)) =>
               c -> HLC (TypedExpr (HLCWeakPtr a))
makeWeakRef = fmap (TypedExpr . fromTypedExpr) . rhsExpr

allocMem :: forall a b c p.
            (Struct p a,
             RHSExpression c b,
             HLCBasicIntType b,HLCNumType b) =>
            Proxy a ->
            c -> HLC (TypedLHS (HLCUniquePtr a))
allocMem _ len = HLC $ do
  symb <- makeHLCSymbol_ "ptr"
  numBytes <- innerHLC $ hlcMul (hlcSizeof (Proxy :: Proxy a)) (rhsExpr len)
  mallocExpr <- callMalloc numBytes
  _ <- innerHLC $ declareStruct (Proxy :: Proxy a)
  let ptrTy = fromTW (hlcType :: TW (HLCUniquePtr a))
      ptr = TypedLHSVar $ TypedVar symb
      consBody = AssignmentStmt (LHSVar symb) mallocExpr
  destBody <- fmap ExpStmt ((innerHLC $ lhsExpr ptr) >>= callFree)
  let cons = HLCBlock [] (StatementList [consBody]) NextLine
      dest = HLCBlock [] (StatementList [destBody]) NextLine
  writeVar $ Variable symb ptrTy Nothing cons dest
  return ptr

makePrimVar :: forall a. (HLCPrimType a) =>
               Proxy a ->
               HLC (TypedLHS a)
makePrimVar _ = HLC $ do
  symb <- makeHLCSymbol_ "primVar"
  writePreproDirs (PreprocessorDirective "#include <stdint.h>")
  let ty = fromTW (hlcType :: TW a)
  writeVar $ Variable symb ty Nothing emptyBlock emptyBlock
  return $ TypedLHSVar $ TypedVar symb

makeLocalStruct :: forall structType p. (Struct p structType) =>
                   Proxy structType ->
                   HLC (TypedLHS structType)
makeLocalStruct _ = HLC $ do
  symb <- makeHLCSymbol_ "stVar"
  consCont <- makeHLCSymbol_ $ makeSafeName "conscont"
  destCont <- makeHLCSymbol_ $ makeSafeName "destcont"
  _ <- innerHLC $ declareStruct (Proxy :: Proxy structType)
  let ty = fromTW (hlcType :: TW structType)
      this = TypedVar symb
  cons <- grabStructBlock $ innerHLC $
    constructor
    (Proxy :: Proxy structType)
    (\fieldName -> return $ TypedLHSElement (TypedLHSVar this) fieldName)
    (SomeContext consCont)
  dest <- grabStructBlock $ innerHLC $
    destructor
    (Proxy :: Proxy structType)
    (SomeContext destCont)
  writeVar $ Variable symb ty Nothing cons dest
  return $ TypedLHSVar this

nullConstructor :: Proxy a -> b -> Context -> HLC Context
nullConstructor _ _ cxt = return cxt

nullDestructor :: Proxy a -> Context -> HLC Context
nullDestructor _ cxt = return cxt


