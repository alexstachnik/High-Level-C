{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LiberalTypeSynonyms #-}


module HighLevelC.HLCCalls where


import Type.Bool

import Data.Typeable

import HighLevelC.BasicTypes

import HighLevelC.CWriter
import HighLevelC.HLC
import HighLevelC.HLCTypes
import Util.Names

data HList :: [*] -> * where
  HNil  :: HList '[]
  (:+:) :: a -> HList t -> HList (a ': t)

infixr 7 :+:

listToExpr' :: (ListToExpr listTy) => HList listTy -> HLC [HLCExpr]
listToExpr' = sequence . listToExpr
class ListToExpr listTy where
  listToExpr :: HList listTy -> [HLC HLCExpr]
instance ListToExpr '[] where
  listToExpr HNil = []
instance (ListToExpr xs) => ListToExpr ((TypedLHS x) ': xs) where
  listToExpr (a :+: b) = (return $ LHSExpr $ untypeLHS a) : listToExpr b
instance (ListToExpr xs) => ListToExpr (HLC (TypedExpr x) ': xs) where
  listToExpr (a :+: b) = (fmap fromTypedExpr a) : listToExpr b
instance (ListToExpr xs) => ListToExpr (HLC (TypedLHS x) ': xs) where
  listToExpr (a :+: b) = (fmap (LHSExpr . untypeLHS) a) : listToExpr b

data SomeFunction fType retType = SomeFunction String

class CallExt (fType :: [*]) retType args (isVariadic :: Bool)
instance (CallExt b retType (HList c) isV) =>
         CallExt (a ': b) retType (HList (TypedLHS a ': c)) isV
instance (CallExt b retType (HList c) isV) =>
         CallExt (a ': b) retType (HList (HLC (TypedLHS a) ': c)) isV
instance (CallExt b retType (HList c) isV) =>
         CallExt (a ': b) retType (HList (HLC (TypedExpr a) ': c)) isV
instance CallExt '[] retType (HList '[]) False
instance CallExt '[] retType any True

callVarFunction :: (ListToExpr args,
                    CallExt fType retType (HList args) True) =>
                   ExtFunction fType retType True -> (HList args) -> HLC (TypedExpr retType)
callVarFunction (ExtFunction symb dirs) argList = do
  HLC $ mapM_ writePreproDir dirs
  untypedArgs <- listToExpr' argList
  return $ TypedExpr $ FunctionCall (LHSExpr $ LHSVar symb) untypedArgs


callExtFunction :: (ListToExpr args,
                    CallExt fType retType (HList args) False) =>
                   ExtFunction fType retType False -> (HList args) -> HLC (TypedExpr retType)
callExtFunction (ExtFunction symb dirs) argList = do
  HLC $ mapM_ writePreproDir dirs
  untypedArgs <- listToExpr' argList
  return $ TypedExpr $ FunctionCall (LHSExpr $ LHSVar symb) untypedArgs

callFunPtr :: (ListToExpr argList,
               CallExt fargs retType (HList argList) False,
               RHSExpression a (FunctionPtr fargs retType)) =>
              a -> (HList argList) -> HLC (TypedExpr retType)
callFunPtr ptr argList = do
  ptr' <- rhsExpr ptr
  untypedArgs <- listToExpr' argList
  return $ TypedExpr $ FunctionCall (LHSExpr $ LHSDeref $ LHSPtr (fromTypedExpr ptr')) untypedArgs
