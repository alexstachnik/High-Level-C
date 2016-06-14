{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HighLevelC.BasicTypes where

import Data.Typeable

import IntermediateLang.ILTypes

import HighLevelC.HLCTypes
import HighLevelC.HLC

import Data.Word

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
  hlcType = TW (PtrType NotConst (BaseType NotConst (ILChar NoSign)))

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

instance Struct IsPassable HLCInt where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCChar where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCDouble where
  constructor _ _ = return
  destructor _ = return
instance Struct NotPassable HLCString where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCVoid where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCInt8 where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCInt16 where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCInt32 where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCInt64 where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCUInt8 where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCUInt16 where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCUInt32 where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCUInt64 where
  constructor _ _ = return
  destructor _ = return
instance Struct IsPassable HLCBool where
  constructor _ _ = return
  destructor _ = return

instance HLCNumType HLCInt
instance HLCNumType HLCChar
instance HLCNumType HLCDouble
instance HLCNumType HLCInt8
instance HLCNumType HLCInt16
instance HLCNumType HLCInt32
instance HLCNumType HLCInt64
instance HLCNumType HLCUInt8
instance HLCNumType HLCUInt16
instance HLCNumType HLCUInt32
instance HLCNumType HLCUInt64
instance HLCNumType HLCBool

charLit :: Word8 -> HLC (TypedExpr HLCChar)
charLit = return . TypedExpr . LitExpr . CharLit

intLit :: Integer -> HLC (TypedExpr HLCInt)
intLit = return . TypedExpr . LitExpr . IntLit

doubleLit :: Double -> HLC (TypedExpr HLCDouble)
doubleLit = return . TypedExpr . LitExpr . DoubleLit

stringLit :: String -> HLC (TypedExpr HLCString)
stringLit = return . TypedExpr . LitExpr . StrLit

void :: TypedExpr HLCVoid
void = TypedExpr Void

