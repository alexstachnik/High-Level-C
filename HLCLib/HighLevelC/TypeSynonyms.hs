{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module HighLevelC.TypeSynonyms where

import GHC.Exts
import HighLevelC.HLCTypes

funcWrap1 :: (RHSExpression x1 x1') =>
             (HLC (TypedExpr x1') ->
              HLC (TypedExpr r)) ->
             x1 -> HLC (TypedExpr r)
funcWrap1 f v1 = f (rhsExpr v1)

funcWrap2 :: (RHSExpression x1 x1',
              RHSExpression x2 x2') =>
             (HLC (TypedExpr x1') ->
              HLC (TypedExpr x2') ->
              HLC (TypedExpr r)) ->
             x1 -> x2 -> HLC (TypedExpr r)
funcWrap2 f v1 v2 = f (rhsExpr v1) (rhsExpr v2) 

funcWrap3 :: (RHSExpression x1 x1',
              RHSExpression x2 x2',
              RHSExpression x3 x3') =>
             (HLC (TypedExpr x1') ->
              HLC (TypedExpr x2') ->
              HLC (TypedExpr x3') ->
              HLC (TypedExpr r)) ->
             x1 -> x2 -> x3 -> HLC (TypedExpr r)
funcWrap3 f v1 v2 v3 = f (rhsExpr v1) (rhsExpr v2)  (rhsExpr v3)

type FuncTyWrap0 r = HLC (TypedExpr r)

type FuncTyWrap1 x1 r = forall x1'.
                        (RHSExpression x1' x1) =>
                        x1' -> HLC (TypedExpr r)
type FuncTyWrap2 x1 x2 r = forall x1' x2'.
                           (RHSExpression x1' x1,
                            RHSExpression x2' x2) =>
                           x1' -> x2' -> HLC (TypedExpr r)

type FuncTyWrap3 x1 x2 x3 r = forall x1' x2' x3'.
                              (RHSExpression x1' x1,
                               RHSExpression x2' x2,
                               RHSExpression x3' x3) =>
                              x1' -> x2' -> x3' -> HLC (TypedExpr r)

type ClassWrap1 (c :: * -> Constraint) (a :: *) =
  c (HLC (TypedExpr a))

type ClassWrap2 (c :: * -> * -> Constraint) (a :: *) (b :: *) =
  c (HLC (TypedExpr a)) (HLC (TypedExpr b))
