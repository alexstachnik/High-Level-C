{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}

module HighLevelC.HLCTypes where

import Language.C.Pretty(Pretty,pretty)
import Text.PrettyPrint.HughesPJ

import Data.List
import Data.Typeable

import qualified Data.Map as M
import qualified Data.Set as S

import Util.Names
import IntermediateLang.ILTypes

data Argument = Argument {argumentName :: HLCSymbol,
                          argumentType :: ILType}
              deriving (Eq,Ord,Show)

data Variable = Variable {variableName :: HLCSymbol,
                          variableType :: ILType,
                          variableArrLen :: Maybe HLCExpr}
              deriving (Eq,Ord,Show)

data StructField = StructField {fieldName :: SafeName,
                                fieldType :: ILType,
                                fieldArrLen :: Maybe Integer}
                 deriving (Eq,Ord,Show)

newtype FuncBaseName = FuncBaseName {fromFuncBaseName :: String}
                     deriving (Eq,Ord,Show)

newtype StructBaseName = StructBaseName {fromStructBaseName :: String}
                       deriving (Eq,Ord,Show)

data FunctionInst = FunctionInst FuncBaseName [ILType]
                  deriving (Eq,Ord,Show)

newtype FuncInstances = FuncInstances {fromFuncInstances :: M.Map FunctionInst HLCSymbol}
                  deriving (Eq,Ord,Show)

data StructInst = StructInst StructBaseName [ILType]
                deriving (Eq,Ord,Show)

newtype StructInstances = StructInstances {fromStructInstances :: S.Set StructInst}
                        deriving (Eq,Ord,Show)

newtype TW a = TW {fromTW :: ILType}

class HLCTypeable a where
  hlcType :: TW a
  structDef :: Maybe (StructDef, Proxy a)

getObjType :: forall a. (HLCTypeable a) => a -> ILType
getObjType _ = fromTW (hlcType :: TW a)
  
newtype TypedVar a = TypedVar {fromTypedVar :: HLCSymbol} deriving (Eq,Ord,Show)

instance (HLCTypeable a) => HLCTypeable (TypedExpr a) where
  hlcType = hlcType
  structDef = structDef

data FunctionProto = FunctionProto ILType HLCSymbol [Argument]
                      deriving (Show,Eq,Ord)

data StructProto = StructProto ILTypeName
                 deriving (Show,Eq,Ord)

data StructDef = StructDef ILTypeName [StructField]
               deriving (Show,Eq,Ord)

data FunctionDef = FunctionDef {functionRetType :: ILType,
                                functionName :: HLCSymbol,
                                functionArguments :: [Argument],
                                functionLocalVars :: [Variable],
                                functionStmts :: [HLCStatement],
                                functionObjectManagers :: [ObjectManager]}
                 deriving (Show,Eq,Ord)

data HLCStatement = HLCBlock [Variable] [ObjectManager] [HLCStatement]
                  | HLCExpStmt HLCExpr
                  | HLCAssignment UntypedLHS HLCExpr
                  | HLCLabel HLCStatement
                  deriving (Eq,Ord,Show)

data UntypedLHS = LHSVar HLCSymbol
                | LHSDeref UntypedLHS
                | LHSDerefPlusOffset UntypedLHS Integer
                | LHSElement UntypedLHS SafeName
                deriving (Eq,Ord,Show)

data HLCExpr = ExpVar HLCSymbol
             | FunctionCall HLCExpr [HLCExpr]
             | LitExpr HLCLit
             | AccessPart HLCExpr SafeName
             | SizeOf ILType
             deriving (Eq,Ord,Show)

data HLCLit = CharLit Char
            | IntLit Integer
            | DoubleLit Double
            | StrLit String
            deriving (Eq,Ord,Show)

newtype HLCPointer a = HLCPointer a

deriving instance (Show a) => Show (HLCPointer a)
deriving instance (Eq a) => Eq (HLCPointer a)
deriving instance (Ord a) => Ord (HLCPointer a)

data TypedLHS a where
  TypedLHSVar :: TypedVar a -> TypedLHS a
  TypedLHSDeref :: TypedLHS (HLCPointer a) -> TypedLHS a
  TypedLHSDerefPlusOffset :: TypedLHS (HLCPointer a) -> Integer -> TypedLHS a
  TypedLHSElement :: (Typeable fieldName,
                      Struct structType fieldName fieldType) =>
                     TypedLHS structType -> Proxy fieldName -> TypedLHS fieldType

data ObjectManager = ObjectManager {constructor :: [HLCStatement],
                                    destructor :: [HLCStatement]}
                   deriving (Show,Eq,Ord)

newtype TypedExpr a = TypedExpr {fromTypedExpr :: HLCExpr}

class (Typeable fieldName) =>
      Struct structType fieldName fieldType | structType fieldName -> fieldType


getFieldName :: forall a. (Typeable a) => Proxy a -> SafeName
getFieldName _ = makeSafeName $ show $ typeRep (Proxy :: Proxy a)

emptyFuncInstances :: FuncInstances
emptyFuncInstances = FuncInstances M.empty

emptyStructInstances :: StructInstances
emptyStructInstances = StructInstances S.empty

makeFuncBaseName :: String -> FuncBaseName
makeFuncBaseName = FuncBaseName . fromSafeName . makeSafeName

makeStructBaseName :: String -> StructBaseName
makeStructBaseName = StructBaseName . fromSafeName . makeSafeName

deriveFuncName :: FuncBaseName -> [ILType] -> SafeName
deriveFuncName (FuncBaseName baseName) args =
  joinSafeNames
  (SafeName baseName :
   map (makeSafeName . trim . show . pretty . writeDecl "") args)

deriveStructName :: StructBaseName -> [ILType] -> ILTypeName
deriveStructName (StructBaseName baseName) args =
  ILTypeName $ fromSafeName $ joinSafeNames
  (SafeName baseName:
   map (makeSafeName . trim . show . pretty . writeDecl "") args)

