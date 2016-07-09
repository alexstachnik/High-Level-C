{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import HighLevelC.Operators
import HighLevelC.HLCCalls

import Language.Haskell.TH

type Var a = HLC (TypedLHS a)

makeVar :: forall a. (HLCTypeable a,
                      Instanciable a (IsPrimitive a)) =>
           HLC (TypedLHS a)
makeVar = HLC $ do
  symb <- makeHLCSymbol_ "var"
  consCont <- makeHLCSymbol_ $ makeSafeName "conscont"
  destCont <- makeHLCSymbol_ $ makeSafeName "destcont"
  writePreproDir (PreprocessorDirective "#include <stdint.h>")
  _ <- innerHLC $ declareObj (Proxy :: Proxy a)
  let ty = fromTW (hlcType :: TW a)
      this = TypedVar symb
  cons <- grabStructBlock $ innerHLC $
    construct
    (Proxy :: Proxy a)
    (return $ TypedLHSVar this)
    (return $ SomeContext consCont)
  dest <- grabStructBlock $ innerHLC $
    destruct
    (Proxy :: Proxy a)
    (return $ TypedLHSVar this)
    (return $ SomeContext destCont)
  writeVar $ Variable symb ty Nothing cons dest
  return $ TypedLHSVar this

nullConstructor :: Proxy a -> b -> HLC Context -> HLC Context
nullConstructor _ _ = id

nullDestructor :: Proxy a -> b -> HLC Context -> HLC Context
nullDestructor _ _ = id


