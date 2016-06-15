{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HighLevelC.Operators where

import HighLevelC.HLCTypes
import HighLevelC.BasicTypes

import Data.Typeable

hlcSizeof :: forall a b. (HLCTypeable a,HLCBasicIntType b) => Proxy a -> HLC (TypedExpr b)
hlcSizeof _ = return $ TypedExpr $ SizeOf $ fromTW (hlcType :: TW a)

hlcBinOp :: (HLCNumType a) =>
            HLCBinOp ->
            HLC (TypedExpr a) ->
            HLC (TypedExpr a) ->
            HLC (TypedExpr a)
hlcBinOp binOp lhs rhs = do
  lhs' <- lhs
  rhs' <- rhs
  return $ TypedExpr $ ExprBinOp binOp (fromTypedExpr lhs') (fromTypedExpr rhs')

hlcAdd :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcAdd = hlcBinOp HLCPlus
hlcSub :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcSub = hlcBinOp HLCMinus
hlcMul :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcMul = hlcBinOp HLCTimes
hlcDiv :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcDiv = hlcBinOp HLCDivide
hlcMod :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcMod = hlcBinOp HLCRem

hlcLAnd :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcLAnd = hlcBinOp HLCLAnd
hlcLOr :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcLOr = hlcBinOp HLCLOr
hlcBitAnd :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcBitAnd = hlcBinOp HLCBitAnd
hlcBitOr :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcBitOr = hlcBinOp HLCBitOr
hlcBitXor :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcBitXor = hlcBinOp HLCBitXor

hlcEqual :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcEqual = hlcBinOp HLCEqual

hlcLT :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcLT = hlcBinOp HLCLT
hlcGT :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcGT = hlcBinOp HLCGT
hlcLEQ :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcLEQ = hlcBinOp HLCLTEQ
hlcGEQ :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcGEQ = hlcBinOp HLCGTEQ
hlcSHL :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcSHL = hlcBinOp HLCSHL
hlcSHR :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcSHR = hlcBinOp HLCSHR

(%%) :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
(%%) = hlcMod

hlcSigNum :: forall a. (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcSigNum val = do
  cond <- (val `hlcLT` hlcFromInteger 0)
  thenExpr :: TypedExpr a <- hlcFromInteger (-1)
  cond2 <- (val `hlcGT` hlcFromInteger 0)
  then2Expr :: TypedExpr a <- hlcFromInteger 1
  elseExpr :: TypedExpr a <- hlcFromInteger 0
  return $ TypedExpr $
    HLCTernary (fromTypedExpr cond) (fromTypedExpr thenExpr) $
    HLCTernary (fromTypedExpr cond2) (fromTypedExpr then2Expr)
    (fromTypedExpr elseExpr)

hlcNegate :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcNegate val = do
  val' <- val
  return $ TypedExpr $ ExprNegate (fromTypedExpr val')

hlcAbs :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcAbs val = do
  cond <- (val `hlcLT` hlcFromInteger 0)
  thenExpr <- (hlcNegate val)
  elseExpr <- val
  return $ TypedExpr $
    HLCTernary (fromTypedExpr cond) (fromTypedExpr thenExpr) (fromTypedExpr elseExpr)

ptrEqual :: (HLCTypeable a, HLCTypeable b) =>
            HLC (TypedExpr (HLCPtr t a)) -> HLC (TypedExpr (HLCPtr t' b)) -> HLC (TypedExpr HLCBool)
ptrEqual lhs rhs = do
  lhs' <- lhs
  rhs' <- rhs
  return $ TypedExpr $ ExprBinOp HLCEqual (fromTypedExpr lhs') (fromTypedExpr rhs')

addrOf :: HLC (TypedVar a) -> HLC (TypedExpr (HLCPtr WeakPtr a))
addrOf var = do
  var' <- var
  return $ lhsExpr $ TypedLHSAddrOf $ TypedLHSVar var'

instance (HLCNumType a) => Num (HLC (TypedExpr a)) where
  m + n = hlcAdd m n
  m - n = hlcSub m n
  m * n = hlcMul m n
  negate = hlcNegate
  abs = hlcAbs
  signum = hlcSigNum
  fromInteger = hlcFromInteger
