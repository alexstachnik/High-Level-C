{-# LANGUAGE EmptyDataDecls #-}

module HighLevelC.BasicTypes where

import IntermediateLang.ILTypes

import HighLevelC.HLCTypes
import HighLevelC.HLC

import Util.Names

data HLCInt
data HLCChar
data HLCDouble
data HLCString
data HLCVoid

instance HLCTypeable HLCInt where
  hlcType = TW (BaseType Signed NotConst ILInt)
  structDef = Nothing

instance HLCTypeable HLCChar where
  hlcType = TW (BaseType Unsigned NotConst ILChar)
  structDef = Nothing

instance HLCTypeable HLCDouble where
  hlcType = TW (BaseType Signed NotConst ILDouble)
  structDef = Nothing

instance HLCTypeable HLCString where
  hlcType = TW (ArrType (BaseType Unsigned NotConst ILChar))
  structDef = Nothing

instance HLCTypeable HLCVoid where
  hlcType = TW (BaseType NoSign NotConst ILVoid)
  structDef = Nothing

makeInt :: String -> HLC (TypedVar HLCInt)
makeInt = makeVar . makeSafeName

makeChar :: String -> HLC (TypedVar HLCChar)
makeChar = makeVar . makeSafeName

makeDouble :: String -> HLC (TypedVar HLCDouble)
makeDouble = makeVar . makeSafeName

makeString :: String -> HLC (TypedVar HLCString)
makeString = makeVar . makeSafeName

makeVoid :: TypedExpr HLCVoid
makeVoid = TypedExpr Void

makeIntLit :: Integer -> TypedExpr HLCInt
makeIntLit = TypedExpr . LitExpr . IntLit

makeCharLit :: Char -> TypedExpr HLCChar
makeCharLit = TypedExpr . LitExpr . CharLit

makeDoubleLit :: Double -> TypedExpr HLCDouble
makeDoubleLit = TypedExpr . LitExpr . DoubleLit

makeStrLit :: String -> TypedExpr HLCString
makeStrLit = TypedExpr . LitExpr . StrLit

