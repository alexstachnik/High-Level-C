{-# LANGUAGE ScopedTypeVariables #-}

module HighLevelC.Operators where

import HighLevelC.HLCTypes
import HighLevelC.BasicTypes

import Data.Typeable

hlcSizeof :: forall a b. (HLCTypeable a,HLCBasicIntType b) => Proxy a -> HLC (TypedExpr b)
hlcSizeof _ = return $ TypedExpr $ SizeOf $ fromTW (hlcType :: TW a)

hlcAdd :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcAdd lhs rhs = do
  lhs' <- lhs
  rhs' <- rhs
  return $ TypedExpr $ ExprBinOp HLCPlus (fromTypedExpr lhs') (fromTypedExpr rhs')

hlcSub :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcSub lhs rhs = do
  lhs' <- lhs
  rhs' <- rhs
  return $ TypedExpr $ ExprBinOp HLCMinus (fromTypedExpr lhs') (fromTypedExpr rhs')

hlcMul :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcMul lhs rhs = do
  lhs' <- lhs
  rhs' <- rhs
  return $ TypedExpr $ ExprBinOp HLCTimes (fromTypedExpr lhs') (fromTypedExpr rhs')

hlcDiv :: (HLCNumType a) => HLC (TypedExpr a) -> HLC (TypedExpr a) -> HLC (TypedExpr a)
hlcDiv lhs rhs = do
  lhs' <- lhs
  rhs' <- rhs
  return $ TypedExpr $ ExprBinOp HLCDivide (fromTypedExpr lhs') (fromTypedExpr rhs')
