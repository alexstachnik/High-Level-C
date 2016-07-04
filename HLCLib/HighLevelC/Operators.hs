
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HighLevelC.Operators where

import HighLevelC.HLCTypes
import HighLevelC.BasicTypes
import HighLevelC.CWriter

import Data.Typeable

hlcSizeof :: forall a b. (HLCTypeable a,HLCBasicIntType b) => Proxy a -> HLC (TypedExpr b)
hlcSizeof _ = return $ TypedExpr $ SizeOf $ fromTW (hlcType :: TW a)





hlcBinOp :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
            HLCBinOp -> a -> b -> HLC (TypedExpr t)
hlcBinOp binOp lhs rhs = do
  lhs' <- rhsExpr lhs
  rhs' <- rhsExpr rhs
  return $ TypedExpr $ ExprBinOp binOp (fromTypedExpr lhs') (fromTypedExpr rhs')

hlcAdd :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcAdd = hlcBinOp HLCPlus
(%+) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%+) = hlcAdd

hlcSub :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcSub = hlcBinOp HLCMinus
(%-) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%-) = hlcSub

hlcMul :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcMul = hlcBinOp HLCTimes
(%*) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%*) = hlcMul

hlcDiv :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcDiv = hlcBinOp HLCDivide
(%/) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%/) = hlcDiv




hlcLogicOp :: (RHSExpression a HLCBool, RHSExpression b HLCBool) =>
              HLCBinOp -> a -> b -> HLC (TypedExpr HLCBool)
hlcLogicOp binOp lhs rhs = do
  lhs' <- rhsExpr lhs
  rhs' <- rhsExpr rhs
  return $ TypedExpr $ ExprBinOp binOp (fromTypedExpr lhs') (fromTypedExpr rhs')

hlcLAnd :: (RHSExpression a HLCBool, RHSExpression b HLCBool) =>
           a -> b -> HLC (TypedExpr HLCBool)
hlcLAnd = hlcLogicOp HLCLAnd
(%&&) :: (RHSExpression a HLCBool, RHSExpression b HLCBool) =>
         a -> b -> HLC (TypedExpr HLCBool)
(%&&) = hlcLAnd

hlcLOr :: (RHSExpression a HLCBool, RHSExpression b HLCBool) =>
           a -> b -> HLC (TypedExpr HLCBool)
hlcLOr = hlcLogicOp HLCLOr
(%||) :: (RHSExpression a HLCBool, RHSExpression b HLCBool) =>
           a -> b -> HLC (TypedExpr HLCBool)
(%||) = hlcLOr


hlcMod :: (HLCBasicIntType t,HLCNumType t,
           RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcMod = hlcBinOp HLCRem
(%%) :: (HLCBasicIntType t,HLCNumType t,
         RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
(%%) = hlcMod

hlcBitAnd :: (HLCBasicIntType t,HLCNumType t,
              RHSExpression a t, RHSExpression b t) =>
             a -> b -> HLC (TypedExpr t)
hlcBitAnd = hlcBinOp HLCBitAnd
(%&) :: (HLCBasicIntType t,HLCNumType t,
         RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%&) = hlcBitAnd

hlcBitOr :: (HLCBasicIntType t,HLCNumType t,
             RHSExpression a t, RHSExpression b t) =>
            a -> b -> HLC (TypedExpr t)
hlcBitOr = hlcBinOp HLCBitOr
(%|) :: (HLCBasicIntType t,HLCNumType t,
         RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%|) = hlcBitOr

hlcBitXor :: (HLCBasicIntType t,HLCNumType t,
              RHSExpression a t, RHSExpression b t) =>
             a -> b -> HLC (TypedExpr t)
hlcBitXor = hlcBinOp HLCBitXor
(%^) :: (HLCBasicIntType t,HLCNumType t,
         RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%^) = hlcBitXor
hlcSHL :: (HLCNumType t,HLCBasicIntType t,
           RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcSHL = hlcBinOp HLCSHL
hlcSHR :: (HLCNumType t,HLCBasicIntType t,
           RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcSHR = hlcBinOp HLCSHR




hlcBoolOp :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
             HLCBinOp -> a -> b -> HLC (TypedExpr HLCBool)
hlcBoolOp binOp lhs rhs = do
  lhs' <- rhsExpr lhs
  rhs' <- rhsExpr rhs
  return $ TypedExpr $ ExprBinOp binOp (fromTypedExpr lhs') (fromTypedExpr rhs')

hlcEqual :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
            a -> b -> HLC (TypedExpr HLCBool)
hlcEqual = hlcBoolOp HLCEqual
(%==) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr HLCBool)
(%==) = hlcEqual

hlcLT :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr HLCBool)
hlcLT = hlcBoolOp HLCLT
(%<) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr HLCBool)
(%<) = hlcLT

hlcGT :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr HLCBool)
hlcGT = hlcBoolOp HLCGT
(%>) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr HLCBool)
(%>) = hlcGT

hlcLEQ :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr HLCBool)
hlcLEQ = hlcBoolOp HLCLTEQ
(%<=) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr HLCBool)
(%<=) = hlcLEQ

hlcGEQ :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr HLCBool)
hlcGEQ = hlcBoolOp HLCGTEQ
(%>=) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr HLCBool)
(%>=) = hlcGEQ

ptrEqual :: (RHSExpression (HLCPtr a) t, RHSExpression (HLCPtr b) t) =>
            (HLCPtr a) -> (HLCPtr b) -> HLC (TypedExpr HLCBool)
ptrEqual lhs rhs = do
  lhs' <- rhsExpr lhs
  rhs' <- rhsExpr rhs
  return $ TypedExpr $ ExprBinOp HLCEqual (fromTypedExpr lhs') (fromTypedExpr rhs')

(%*==) :: (RHSExpression (HLCPtr a) t, RHSExpression (HLCPtr b) t) =>
          (HLCPtr a) -> (HLCPtr b) -> HLC (TypedExpr HLCBool)
(%*==) = ptrEqual



infixl 9 %.
infixl 9 %@

class LeftOrRightOp (lhs :: *) where
  type Result (lhs :: *) (a :: *)
  type StructType lhs
  type PtrType lhs
  (%.) :: (StructFieldClass (StructType lhs) fieldName fieldType, Typeable fieldName) =>
           lhs -> Proxy fieldName -> (Result lhs) fieldType
  deref :: (PtrType lhs) -> Result lhs (StructType lhs)
  (%@) :: (RHSExpression b b', HLCBasicIntType b') => (PtrType lhs) -> b -> Result lhs (StructType lhs)

instance LeftOrRightOp (HLC (TypedExpr a)) where
  type Result (HLC (TypedExpr a)) b = HLC (TypedExpr b)
  type StructType (HLC (TypedExpr a)) = a
  type PtrType (HLC (TypedExpr a)) = HLC (TypedExpr (HLCPtr a))
  st %. field = readElt st field
  deref = fmap (TypedExpr . LHSExpr . untypeLHS . TypedLHSDeref . TypedLHSPtr)
  ptr %@ n = do
    ptr' <- ptr
    n' <- rhsExpr n
    return $ TypedExpr $ LHSExpr $ untypeLHS $
      TypedLHSDerefPlusOffset (TypedLHSPtr ptr') n'


instance LeftOrRightOp (HLC (TypedLHS a)) where
  type Result (HLC (TypedLHS a)) b = HLC (TypedLHS b)
  type StructType (HLC (TypedLHS a)) = a
  type PtrType (HLC (TypedLHS a)) = HLC (TypedLHS (HLCPtr a))
  struct %. field = do
    struct' <- struct
    return $ TypedLHSElement struct' field
  deref var = var >>= (return . TypedLHSDeref)
  ptr %@ n = do
    ptr' <- ptr
    n' <- rhsExpr n
    return $ TypedLHSDerefPlusOffset ptr' n'

instance LeftOrRightOp (TypedLHS a) where
  type Result (TypedLHS a) b = HLC (TypedLHS b)
  type StructType (TypedLHS a) = a
  type PtrType (TypedLHS a) = TypedLHS (HLCPtr a)
  struct %. field = return $ TypedLHSElement struct field
  deref var = return $ TypedLHSDeref var
  ptr %@ n = do
    n' <- rhsExpr n
    return $ TypedLHSDerefPlusOffset ptr n'


hlcNegate :: (RHSExpression a b,
              HLCNumType b) =>
             a -> HLC (TypedExpr b)
hlcNegate val = do
  val' <- rhsExpr val
  return $ TypedExpr $ ExprNegate (fromTypedExpr val')

hlcAbs :: (RHSExpression a b,
           HLCNumType b) =>
          a -> HLC (TypedExpr b)
hlcAbs val = do
  cond <- (rhsExpr val `hlcLT` hlcFromInteger 0)
  thenExpr <- (hlcNegate $ rhsExpr val)
  elseExpr <- rhsExpr val
  return $ TypedExpr $
    HLCTernary (fromTypedExpr cond) (fromTypedExpr thenExpr) (fromTypedExpr elseExpr)



addrOf :: HLC (TypedVar a) -> HLC (TypedExpr (HLCPtr a))
addrOf var = do
  var' <- var
  lhsExpr $ TypedLHSAddrOf $ TypedLHSVar var'


assignVar :: forall a a' b.
             (LHSExpression a a',
              HLCTypeable a', Passability a' ~ IsPassable,
              RHSExpression b a') =>
             a -> b -> HLC ()
assignVar lhs rhs = do
  lhs' <- hlcLHSExpr lhs
  rhs' <- rhsExpr rhs
  HLC $ writeStmt $ AssignmentStmt (untypeLHS lhs') (fromTypedExpr rhs')

infixr 0 =:

(=:) :: forall a a' b.
        (LHSExpression a a',
         HLCTypeable a', Passability a' ~ IsPassable,
         RHSExpression b a') =>
        a -> b -> HLC ()
(=:) = assignVar
