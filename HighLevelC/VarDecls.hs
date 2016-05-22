{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module HighLevelC.VarDecls where

import Data.Typeable

import Util.Names

import HighLevelC.HLCTypes
import HighLevelC.HLC
import HighLevelC.BasicTypes
import HighLevelC.CWriter

makePrimVar :: forall a. (HLCPrimType a) =>
               SafeName ->
               (TypedVar a -> [HLCStatement]) ->
               (TypedVar a -> [HLCStatement]) ->
               HLC (TypedVar a)
makePrimVar name cons dest = HLC $ do
  symb <- makeHLCSymbol_ name
  let ty = fromTW (hlcType :: TW a)
  writeVar $ Variable symb ty Nothing (cons (TypedVar symb)) (dest (TypedVar symb))
  return $ TypedVar symb

makeLocalStruct :: forall a. (Struct a) =>
                   SafeName ->
                   (TypedVar a -> [HLCStatement]) ->
                   (TypedVar a -> [HLCStatement]) ->
                   HLC (TypedVar a)
makeLocalStruct name cons dest = HLC $ do
  symb <- makeHLCSymbol_ name
  msymb <- lookupStruct (getStructName (Proxy :: Proxy a))
  case msymb of
    (Just symb) -> return ()
    Nothing -> declareStruct (Proxy :: Proxy a) >> return ()
  let ty = fromTW (hlcType :: TW a)
  writeVar $ Variable symb ty Nothing (cons (TypedVar symb)) (dest (TypedVar symb))
  return $ TypedVar symb

assignVar :: forall a. (HLCTypeable a, Passability a ~ IsPassable) =>
             TypedLHS a ->
             TypedExpr a ->
             HLC ()
assignVar lhs rhs = HLC $ writeStmt $ AssignmentStmt (untypeLHS lhs) (fromTypedExpr rhs)

