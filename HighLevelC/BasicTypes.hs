{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HighLevelC.BasicTypes where

import Data.Typeable

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
data HLCBool

instance HLCTypeable HLCInt where
  hlcType = TW (BaseType NotConst (ILInt Signed))

instance HLCTypeable HLCChar where
  hlcType = TW (BaseType NotConst (ILChar NoSign))

instance HLCTypeable HLCDouble where
  hlcType = TW (BaseType NotConst (ILDouble Signed))

instance HLCTypeable HLCString where
  hlcType = TW (ArrType (BaseType NotConst (ILChar NoSign)))

instance HLCTypeable HLCVoid where
  hlcType = TW (BaseType NotConst ILVoid)

instance HLCTypeable HLCInt8 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "int8_t"))

instance HLCTypeable HLCInt16 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "int16_t"))

instance HLCTypeable HLCInt32 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "int32_t"))

instance HLCTypeable HLCInt64 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "int64_t"))

instance HLCTypeable HLCUInt8 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "uint8_t"))

instance HLCTypeable HLCUInt16 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "uint16_t"))

instance HLCTypeable HLCUInt32 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "uint32_t"))

instance HLCTypeable HLCUInt64 where
  hlcType = TW (BaseType NotConst (ILNewName $ ILTypeName "uint64_t"))

instance HLCTypeable HLCBool where
  hlcType = TW (BaseType NotConst (ILChar NoSign))

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
instance HLCBasicIntType HLCBool


fromIntType :: forall a b. (HLCBasicIntType a, HLCBasicIntType b) => TypedExpr a -> TypedExpr b
fromIntType = TypedExpr . HLCCast (fromTW (hlcType :: TW b)) . fromTypedExpr


instance HLCPrimType HLCInt
instance HLCPrimType HLCChar
instance HLCPrimType HLCDouble
instance HLCPrimType HLCString
instance HLCPrimType HLCVoid
instance HLCPrimType HLCInt8
instance HLCPrimType HLCInt16
instance HLCPrimType HLCInt32
instance HLCPrimType HLCInt64
instance HLCPrimType HLCUInt8
instance HLCPrimType HLCUInt16
instance HLCPrimType HLCUInt32
instance HLCPrimType HLCUInt64
instance HLCPrimType HLCBool


