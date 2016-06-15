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

makeWeakRef :: HLC (TypedExpr (HLCPtr p a)) -> HLC (TypedExpr (HLCWeakPtr a))
makeWeakRef = fmap (TypedExpr . fromTypedExpr)

mallocExpr :: (HLCBasicIntType b) => TypedExpr b -> HLCExpr
mallocExpr len = FunctionCall (LHSExpr $ LHSVar $ fromExtFunction malloc) [fromTypedExpr len]

--maybeDeclareStruct :: forall a. (HLCTypeable a) =>
--                      Proxy a -> HLC_ ()
--maybeDeclareStruct _ =
--  case eqT :: Maybe (forall p structType. (Struct p structType) => structType :~: a) of
--    (Just (Refl))  ->
--      innerHLC $ declareStruct (Proxy :: Proxy a) >>
--      return ()
--    Nothing ->
--      return ()
  

allocMem :: forall a b p. (Struct p a,HLCBasicIntType b,HLCNumType b) =>
            SafeName -> HLC (TypedExpr b) -> HLC (TypedVar (HLCUniquePtr a))
allocMem name len = HLC $ do
  symb <- makeHLCSymbol_ name
  numBytes <- innerHLC $ hlcMul (hlcSizeof (Proxy :: Proxy a)) len
  innerHLC $ declareStruct (Proxy :: Proxy a)
  let ptrTy = fromTW (hlcType :: TW (HLCUniquePtr a))
      consBody = AssignmentStmt (LHSVar symb) (mallocExpr numBytes)
      destBody = ExpStmt $
        FunctionCall (LHSExpr $ LHSVar $ fromExtFunction freeMem) [LHSExpr $ LHSVar symb]
      cons = HLCBlock [] (StatementList [consBody]) NextLine
      dest = HLCBlock [] (StatementList [destBody]) NextLine
  writeVar $ Variable symb ptrTy Nothing cons dest
  return $ TypedVar symb

makePrimVar :: forall a. (HLCPrimType a) =>
               SafeName ->
               HLC (TypedVar a)
makePrimVar name = HLC $ do
  symb <- makeHLCSymbol_ name
  let ty = fromTW (hlcType :: TW a)
  writeVar $ Variable symb ty Nothing emptyBlock emptyBlock
  return $ TypedVar symb

makeLocalStruct :: forall structType p. (Struct p structType) =>
                   SafeName ->
                   HLC (TypedVar structType)
makeLocalStruct name = HLC $ do
  symb <- makeHLCSymbol_ name
  consCont <- makeHLCSymbol_ $ makeSafeName "conscont"
  destCont <- makeHLCSymbol_ $ makeSafeName "destcont"
  innerHLC $ declareStruct (Proxy :: Proxy structType)
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
  return this

assignVar :: forall a. (HLCTypeable a, Passability a ~ IsPassable) =>
             TypedLHS a ->
             HLC (TypedExpr a) ->
             HLC ()
assignVar lhs rhs = do
  rhs' <- rhs
  HLC $ writeStmt $ AssignmentStmt (untypeLHS lhs) (fromTypedExpr rhs')

(=:) :: forall a. (HLCTypeable a, Passability a ~ IsPassable) =>
        TypedLHS a ->
        HLC (TypedExpr a) ->
        HLC ()
(=:) = assignVar
