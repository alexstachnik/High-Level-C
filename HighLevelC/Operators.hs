{-# LANGUAGE ScopedTypeVariables #-}

module HighLevelC.Operators where

import HighLevelC.HLCTypes
import HighLevelC.BasicTypes

import Data.Typeable

hlcSizeof :: forall a b. (HLCTypeable a,HLCBasicIntType b) => Proxy a -> TypedExpr b
hlcSizeof _ = TypedExpr $ SizeOf $ fromTW (hlcType :: TW a)

hlcAdd :: (HLCNumType a) => TypedExpr a -> TypedExpr a -> TypedExpr a
hlcAdd lhs rhs = TypedExpr $ ExprBinOp HLCPlus (fromTypedExpr lhs) (fromTypedExpr rhs)

hlcSub :: (HLCNumType a) => TypedExpr a -> TypedExpr a -> TypedExpr a
hlcSub lhs rhs = TypedExpr $ ExprBinOp HLCMinus (fromTypedExpr lhs) (fromTypedExpr rhs)

hlcMul :: (HLCNumType a) => TypedExpr a -> TypedExpr a -> TypedExpr a
hlcMul lhs rhs = TypedExpr $ ExprBinOp HLCTimes (fromTypedExpr lhs) (fromTypedExpr rhs)

hlcDiv :: (HLCNumType a) => TypedExpr a -> TypedExpr a -> TypedExpr a
hlcDiv lhs rhs = TypedExpr $ ExprBinOp HLCDivide (fromTypedExpr lhs) (fromTypedExpr rhs)

