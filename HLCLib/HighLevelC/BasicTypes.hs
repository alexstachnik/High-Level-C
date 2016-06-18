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
data HLCInt8
data HLCInt16
data HLCInt32
data HLCInt64
data HLCUInt8
data HLCUInt16
data HLCUInt32
data HLCUInt64
data HLCBool

type Type_Int = HLC (TypedExpr HLCInt)
type Type_Char = HLC (TypedExpr HLCChar)
type Type_Double = HLC (TypedExpr HLCDouble)
type Type_String = HLC (TypedExpr HLCString)
type Type_Int8 = HLC (TypedExpr HLCInt8)
type Type_Int16 = HLC (TypedExpr HLCInt16)
type Type_Int32 = HLC (TypedExpr HLCInt32)
type Type_Int64 = HLC (TypedExpr HLCInt64)
type Type_UInt8 = HLC (TypedExpr HLCUInt8)
type Type_UInt16 = HLC (TypedExpr HLCUInt16)
type Type_UInt32 = HLC (TypedExpr HLCUInt32)
type Type_UInt64 = HLC (TypedExpr HLCUInt64)
type Type_Bool = HLC (TypedExpr HLCBool)

type_Int = Proxy :: Proxy HLCInt
type_Char = Proxy :: Proxy HLCChar
type_Double = Proxy :: Proxy HLCDouble
type_String = Proxy :: Proxy HLCString
type_Int8 = Proxy :: Proxy HLCInt8
type_Int16 = Proxy :: Proxy HLCInt16
type_Int32 = Proxy :: Proxy HLCInt32
type_Int64 = Proxy :: Proxy HLCInt64
type_UInt8 = Proxy :: Proxy HLCUInt8
type_UInt16 = Proxy :: Proxy HLCUInt16
type_UInt32 = Proxy :: Proxy HLCUInt32
type_UInt64 = Proxy :: Proxy HLCUInt64
type_Bool = Proxy :: Proxy HLCBool

instance HLCTypeable HLCInt where
  hlcType = TW (BaseType NotConst (ILInt Signed))

instance HLCTypeable HLCChar where
  hlcType = TW (BaseType NotConst (ILChar NoSign))

instance HLCTypeable HLCDouble where
  hlcType = TW (BaseType NotConst (ILDouble Signed))

instance HLCTypeable HLCString where
  hlcType = TW (PtrType NotConst (BaseType NotConst (ILChar NoSign)))

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


fromIntType :: forall a b. (HLCBasicIntType a, HLCBasicIntType b) => HLC (TypedExpr a) -> HLC (TypedExpr b)
fromIntType = fmap (TypedExpr . HLCCast (fromTW (hlcType :: TW b)) . fromTypedExpr)


instance HLCPrimType HLCInt
instance HLCPrimType HLCChar
instance HLCPrimType HLCDouble
instance HLCPrimType HLCString
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
  fieldList _ = []
instance Struct IsPassable HLCChar where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCDouble where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct NotPassable HLCString where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCInt8 where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCInt16 where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCInt32 where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCInt64 where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCUInt8 where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCUInt16 where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCUInt32 where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCUInt64 where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []
instance Struct IsPassable HLCBool where
  constructor _ _ = return
  destructor _ = return
  fieldList _ = []

instance HLCNumType HLCInt where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCChar where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCDouble where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCInt8 where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCInt16 where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCInt32 where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCInt64 where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCUInt8 where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCUInt16 where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCUInt32 where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCUInt64 where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit
instance HLCNumType HLCBool where
  hlcFromInteger = return . TypedExpr . LitExpr . IntLit

charLit :: Word8 -> HLC (TypedExpr HLCChar)
charLit = return . TypedExpr . LitExpr . CharLit

intLit :: Integer -> HLC (TypedExpr HLCInt)
intLit = return . TypedExpr . LitExpr . IntLit

doubleLit :: Double -> HLC (TypedExpr HLCDouble)
doubleLit = return . TypedExpr . LitExpr . DoubleLit

stringLit :: String -> HLC (TypedExpr HLCString)
stringLit = return . TypedExpr . LitExpr . StrLit

void :: HLC (TypedExpr HLCVoid)
void = return $ TypedExpr Void


