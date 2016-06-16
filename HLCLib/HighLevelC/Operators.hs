{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HighLevelC.Operators where

import HighLevelC.HLCTypes
import HighLevelC.BasicTypes

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
hlcSub :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcSub = hlcBinOp HLCMinus
hlcMul :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcMul = hlcBinOp HLCTimes
hlcDiv :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcDiv = hlcBinOp HLCDivide
hlcMod :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcMod = hlcBinOp HLCRem

hlcLAnd :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
           a -> b -> HLC (TypedExpr t)
hlcLAnd = hlcBinOp HLCLAnd
hlcLOr :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcLOr = hlcBinOp HLCLOr
hlcBitAnd :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
             a -> b -> HLC (TypedExpr t)
hlcBitAnd = hlcBinOp HLCBitAnd
hlcBitOr :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
            a -> b -> HLC (TypedExpr t)
hlcBitOr = hlcBinOp HLCBitOr
hlcBitXor :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
             a -> b -> HLC (TypedExpr t)
hlcBitXor = hlcBinOp HLCBitXor

hlcEqual :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
            a -> b -> HLC (TypedExpr t)
hlcEqual = hlcBinOp HLCEqual

hlcLT :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr t)
hlcLT = hlcBinOp HLCLT
hlcGT :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr t)
hlcGT = hlcBinOp HLCGT
hlcLEQ :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcLEQ = hlcBinOp HLCLTEQ
hlcGEQ :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcGEQ = hlcBinOp HLCGTEQ
hlcSHL :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcSHL = hlcBinOp HLCSHL
hlcSHR :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
          a -> b -> HLC (TypedExpr t)
hlcSHR = hlcBinOp HLCSHR

(%+) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
m %+ n = hlcAdd m n
(%-) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
m %- n = hlcSub m n
(%*) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
m %* n = hlcMul m n
(%/) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%/) = hlcDiv
(%%) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%%) = hlcMod
(%&&) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr t)
(%&&) = hlcLAnd
(%||) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr t)
(%||) = hlcLOr
(%&) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%&) = hlcBitAnd
(%|) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%|) = hlcBitOr
(%^) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%^) = hlcBitXor
(%==) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr t)
(%==) = hlcEqual
(%*==) :: (RHSExpression (HLCPtr p a) t, RHSExpression (HLCPtr p' b) t) =>
          (HLCPtr p a) -> (HLCPtr p' b) -> HLC (TypedExpr HLCBool)
(%*==) = ptrEqual
(%<) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%<) = hlcLT
(%>) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
        a -> b -> HLC (TypedExpr t)
(%>) = hlcGT
(%<=) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr t)
(%<=) = hlcLEQ
(%>=) :: (HLCNumType t,RHSExpression a t, RHSExpression b t) =>
         a -> b -> HLC (TypedExpr t)
(%>=) = hlcGEQ


(%.) :: forall structType fieldName fieldType p.
        (StructFieldClass p structType fieldName fieldType, Typeable fieldName) =>
        HLC (TypedExpr structType) ->
        Proxy fieldName ->
        HLC (TypedExpr fieldType)
(%.) = readElt

rderef :: HLC (TypedExpr (HLCPtr b a)) -> HLC (TypedExpr a)
rderef = fmap (TypedExpr . LHSExpr . untypeLHS . TypedLHSDeref . TypedLHSPtr)

(%@) :: (HLCBasicIntType c) =>
        HLC (TypedExpr (HLCPtr b a)) ->
        HLC (TypedExpr c) ->
        HLC (TypedExpr a)
(%@) ptr n = do
  ptr' <- ptr
  n' <- n
  return $ TypedExpr $ LHSExpr $ untypeLHS $
    TypedLHSDerefPlusOffset (TypedLHSPtr ptr') n'

($.) :: StructFieldClass p structType fieldName fieldType =>
        TypedLHS structType -> Proxy fieldName -> TypedLHS fieldType
($.) = TypedLHSElement

lderef :: TypedLHS (HLCPtr b a) -> TypedLHS a
lderef = TypedLHSDeref

($@) :: (HLCBasicIntType c) =>
           TypedLHS (HLCPtr b a) ->
           TypedExpr c ->
           TypedLHS a
($@) = TypedLHSDerefPlusOffset

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

ptrEqual :: (RHSExpression (HLCPtr p a) t, RHSExpression (HLCPtr p' b) t) =>
            (HLCPtr p a) -> (HLCPtr p' b) -> HLC (TypedExpr HLCBool)
ptrEqual lhs rhs = do
  lhs' <- rhsExpr lhs
  rhs' <- rhsExpr rhs
  return $ TypedExpr $ ExprBinOp HLCEqual (fromTypedExpr lhs') (fromTypedExpr rhs')

addrOf :: HLC (TypedVar a) -> HLC (TypedExpr (HLCPtr WeakPtr a))
addrOf var = do
  var' <- var
  lhsExpr $ TypedLHSAddrOf $ TypedLHSVar var'

