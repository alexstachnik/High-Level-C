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

(%.) :: (RHSExpression a structType,
         StructFieldClass p structType fieldName fieldType,
         Typeable fieldName) =>
        a ->
        Proxy fieldName ->
        HLC (TypedExpr fieldType)
st %. field = readElt (rhsExpr st) field

rderef :: (RHSExpression c (HLCPtr a)) =>
          c -> HLC (TypedExpr a)
rderef = fmap (TypedExpr . LHSExpr . untypeLHS . TypedLHSDeref . TypedLHSPtr) . rhsExpr

infixl 9 %@

(%@) :: (RHSExpression a (HLCPtr a'),
         RHSExpression b b',
         HLCBasicIntType b') =>
        a -> b -> HLC (TypedExpr a')
(%@) ptr n = do
  ptr' <- rhsExpr ptr
  n' <- rhsExpr n
  return $ TypedExpr $ LHSExpr $ untypeLHS $
    TypedLHSDerefPlusOffset (TypedLHSPtr ptr') n'

infixl 9 $.

($.) :: (LHSExpression a structType,
         StructFieldClass p structType fieldName fieldType) =>
        a -> Proxy fieldName -> HLC (TypedLHS fieldType)
($.) struct field = do
  struct' <- hlcLHSExpr struct
  return $ TypedLHSElement struct' field

lderef :: (LHSExpression a (HLCPtr a')) =>
          a -> HLC (TypedLHS a')
lderef var = do
  var' <- hlcLHSExpr var
  return $ TypedLHSDeref var'

infixl 9 $@

($@) :: (LHSExpression a (HLCPtr a'),
         RHSExpression b b',
         HLCBasicIntType b') =>
        a -> b -> HLC (TypedLHS a')
($@) lhs rhs = do
  lhs' <- hlcLHSExpr lhs
  rhs' <- rhsExpr rhs
  return $ TypedLHSDerefPlusOffset lhs' rhs'


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
