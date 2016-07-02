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
import HighLevelC.HLCCalls

import Language.Haskell.TH

makeWeakRef :: (RHSExpression c (HLCPtr p a)) =>
               c -> HLC (TypedLHS (HLCWeakPtr a))
makeWeakRef = fmap (TypedLHSPtr . TypedExpr . fromTypedExpr) . rhsExpr

freeMemHelper :: (HLCTypeable a) =>
                 TypedLHS (HLCWeakPtr a) -> HLC_ HLCStatement
freeMemHelper ptr = innerHLC $ do
  expr <- callExt1 freeMem ptr NilArg
  return $ ExpStmt $ fromTypedExpr expr

allocMem1 :: forall a. (HLCTypeable a,
                        Instanciable a (IsPrimitive a)) =>
             Proxy a -> HLC (TypedLHS (HLCUniquePtr a))
allocMem1 p = allocMem p (intLit 1)

allocMem :: forall a b c.
            (HLCTypeable a,
             Instanciable a (IsPrimitive a),
             RHSExpression c b,
             HLCBasicIntType b,HLCNumType b) =>
            Proxy a ->
            c -> HLC (TypedLHS (HLCUniquePtr a))
allocMem _ len = HLC $ do
  symb <- makeHLCSymbol_ "ptr"
  let numBytes = fromIntType $ hlcMul (hlcSizeof (Proxy :: Proxy a)) (rhsExpr len)
  mallocExpr :: TypedExpr (HLCWeakPtr HLCVoid) <- innerHLC $ callExt1 malloc numBytes NilArg
  _ <- innerHLC $ declareObj (Proxy :: Proxy a)
  let ptrTy = fromTW (hlcType :: TW (HLCUniquePtr a))
      ptr = TypedLHSVar $ TypedVar symb :: TypedLHS (HLCUniquePtr a)
      consBody = AssignmentStmt (LHSVar symb) $ fromTypedExpr mallocExpr
  weakRef :: TypedLHS (HLCWeakPtr a) <- innerHLC $ makeWeakRef ptr
  destBody <- freeMemHelper weakRef
  let cons = HLCBlock [] (StatementList [consBody]) NextLine
      dest = HLCBlock [] (StatementList [destBody]) NextLine
  writeVar $ Variable symb ptrTy Nothing cons dest
  return ptr


makeVar :: forall a. (HLCTypeable a,
                      Instanciable a (IsPrimitive a)) =>
           Proxy a ->
           HLC (TypedLHS a)
makeVar _ = HLC $ do
  symb <- makeHLCSymbol_ "var"
  consCont <- makeHLCSymbol_ $ makeSafeName "conscont"
  destCont <- makeHLCSymbol_ $ makeSafeName "destcont"
  writePreproDir (PreprocessorDirective "#include <stdint.h>")
  _ <- innerHLC $ declareObj (Proxy :: Proxy a)
  let ty = fromTW (hlcType :: TW a)
      this = TypedVar symb
  cons <- grabStructBlock $ innerHLC $
    construct
    (Proxy :: Proxy a)
    (return $ TypedLHSVar this)
    (SomeContext consCont)
  dest <- grabStructBlock $ innerHLC $
    destruct
    (Proxy :: Proxy a)
    (SomeContext destCont)
  writeVar $ Variable symb ty Nothing cons dest
  return $ TypedLHSVar this

nullConstructor :: Proxy a -> b -> Context -> HLC Context
nullConstructor _ _ cxt = return cxt

nullDestructor :: Proxy a -> Context -> HLC Context
nullDestructor _ cxt = return cxt


