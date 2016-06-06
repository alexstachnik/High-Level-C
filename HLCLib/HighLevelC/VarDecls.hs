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
             TypedExpr a ->
             HLC ()
assignVar lhs rhs = HLC $ writeStmt $ AssignmentStmt (untypeLHS lhs) (fromTypedExpr rhs)

