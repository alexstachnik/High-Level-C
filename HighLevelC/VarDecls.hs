{-# LANGUAGE FlexibleContexts #-}
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
               HLC (TypedVar a)
makePrimVar name = HLC $ do
  symb <- makeHLCSymbol_ name
  let ty = fromTW (hlcType :: TW a)
  writeVar $ Variable symb ty Nothing emptyBlock emptyBlock
  return $ TypedVar symb

makeLocalStruct :: forall structType. (Struct IsPassable structType) =>
                   SafeName ->
                   HLC (TypedVar structType)
makeLocalStruct name = HLC $ do
  symb <- makeHLCSymbol_ name
  msymb <- lookupStruct (getStructName (Proxy :: Proxy structType))
  case msymb of
    (Just symb) -> return ()
    Nothing -> declareStruct (Proxy :: Proxy structType) >> return ()
  let ty = fromTW (hlcType :: TW structType)
  cons <- grabStructBlock $ innerHLC $
    constructor (Proxy :: Proxy structType) (HLC . makeStructField (Proxy :: Proxy structType))
  dest <- grabStructBlock $ innerHLC $ destructor (Proxy :: Proxy structType)
  writeVar $ Variable symb ty Nothing cons dest
  return $ TypedVar symb

assignVar :: forall a. (HLCTypeable a, Passability a ~ IsPassable) =>
             TypedLHS a ->
             TypedExpr a ->
             HLC ()
assignVar lhs rhs = HLC $ writeStmt $ AssignmentStmt (untypeLHS lhs) (fromTypedExpr rhs)

