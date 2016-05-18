{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
data HLCInt8
data HLCInt16
data HLCInt32
data HLCInt64
data HLCUInt8
data HLCUInt16
data HLCUInt32
data HLCUInt64

class (HLCTypeable a) => HLCBasicIntType a

instance HLCTypeable HLCInt where
  hlcType = TW (BaseType NotConst (ILInt Signed))
  structDef = Nothing

instance HLCTypeable HLCChar where
  hlcType = TW (BaseType NotConst (ILChar NoSign))
  structDef = Nothing

instance HLCTypeable HLCDouble where
  hlcType = TW (BaseType NotConst (ILDouble Signed))
  structDef = Nothing

instance HLCTypeable HLCString where
  hlcType = TW (ArrType (BaseType NotConst (ILChar NoSign)))
  structDef = Nothing

instance HLCTypeable HLCVoid where
  hlcType = TW (BaseType NotConst ILVoid)
  structDef = Nothing

instance HLCTypeable HLCInt8 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "int8_t"))
  structDef = Nothing

instance HLCTypeable HLCInt16 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "int16_t"))
  structDef = Nothing

instance HLCTypeable HLCInt32 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "int32_t"))
  structDef = Nothing

instance HLCTypeable HLCInt64 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "int64_t"))
  structDef = Nothing

instance HLCTypeable HLCUInt8 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "uint8_t"))
  structDef = Nothing

instance HLCTypeable HLCUInt16 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "uint16_t"))
  structDef = Nothing

instance HLCTypeable HLCUInt32 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "uint32_t"))
  structDef = Nothing

instance HLCTypeable HLCUInt64 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "uint64_t"))
  structDef = Nothing

instance HLCBasicIntType HLCInt
instance HLCBasicIntType HLCInt8
instance HLCBasicIntType HLCInt16
instance HLCBasicIntType HLCInt32
instance HLCBasicIntType HLCInt64
instance HLCBasicIntType HLCUInt8
instance HLCBasicIntType HLCUInt16
instance HLCBasicIntType HLCUInt32
instance HLCBasicIntType HLCUInt64
instance HLCBasicIntType HLCChar


fromIntType :: forall a b. (HLCBasicIntType a, HLCBasicIntType b) => TypedExpr a -> TypedExpr b
fromIntType = TypedExpr . HLCCast (fromTW (hlcType :: TW b)) . fromTypedExpr

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

