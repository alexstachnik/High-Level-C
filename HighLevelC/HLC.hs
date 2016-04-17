{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module HighLevelC.HLC where

import Control.Monad.State
import Control.Monad.Writer(WriterT,execWriterT,MonadWriter,tell)
import Control.Monad.Identity(Identity(runIdentity))

import Language.C.Syntax.AST(CExtDecl)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq

import Data.Maybe(mapMaybe,listToMaybe)

import Data.Typeable

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import HighLevelC.HLCTypes
import HighLevelC.CWriter
import Util.THUtil
import Util.Names
import IntermediateLang.ILTypes


newtype FuncNameTbl = FuncNameTbl {fromFuncNameTbl :: S.Set FuncBaseName}
                      deriving (Show)

newtype StructNameTbl = StructNameTbl {fromStructNameTbl :: S.Set StructBaseName}
                      deriving (Show)

data CodeState = CodeState {funcInstances :: FuncInstances,
                            structInstances :: StructInstances,
                            uidState :: Integer}
                 deriving (Show)

newtype HLC_ a = HLC_ {
  runHLC_ :: (WriterT CWriter
              (StateT CodeState Identity) a)}
              deriving (Monad,
                        Applicative,
                        Functor,
                        MonadState CodeState,
                        MonadWriter CWriter)

newtype HLC a = HLC {innerHLC :: HLC_ a}
              deriving (Monad,Applicative,Functor)

initState = CodeState {funcInstances = emptyFuncInstances,
                       structInstances = emptyStructInstances,
                       uidState = 0}

runInnerHLC :: HLC_ () -> CWriter
runInnerHLC c =
  runIdentity $ evalStateT (execWriterT $ runHLC_ c) initState

runOuterHLC :: HLC () -> CWriter
runOuterHLC = runInnerHLC . innerHLC

newUID :: HLC_ Integer
newUID = do
  (CodeState {..}) <- get
  put (CodeState {uidState=uidState+1,..})
  return uidState

makeHLCSymbol :: SafeName -> HLC HLCSymbol
makeHLCSymbol = HLC . makeHLCSymbol_

makeHLCSymbol_ :: SafeName -> HLC_ HLCSymbol
makeHLCSymbol_ safeName = do
  uid <- newUID
  return $ HLCSymbol uid safeName

lookupFunc :: FunctionInst -> HLC_ (Maybe HLCSymbol)
lookupFunc x = gets funcInstances >>= (return . M.lookup x . fromFuncInstances)

lookupStruct :: StructInst -> HLC_ Bool
lookupStruct x = gets structInstances >>= (return . S.member x. fromStructInstances)

addFuncInst :: FunctionInst -> HLCSymbol -> HLC_ ()
addFuncInst inst symb = modify $ \(CodeState {..}) ->
  CodeState {funcInstances = FuncInstances $ M.insert inst symb $ fromFuncInstances funcInstances,
             ..}

addStructInst :: StructInst -> HLC_ ()
addStructInst inst = modify $ \(CodeState {..}) ->
  CodeState {structInstances = StructInstances $ S.insert inst $ fromStructInstances structInstances,
             ..}

declareFuncInst :: FuncBaseName -> [ILType] -> HLC_ HLCSymbol
declareFuncInst baseName args = do
  let newName = deriveFuncName baseName args
  symb <- makeHLCSymbol_ newName
  addFuncInst (FunctionInst baseName args) symb
  return symb

declareStructInst :: StructBaseName -> [ILType] -> HLC_ ILTypeName
declareStructInst baseName args = do
  addStructInst $ StructInst baseName args
  return $ deriveStructName baseName args

tryReserveFunc :: FuncBaseName -> Q Bool
tryReserveFunc name = do
  mtbl <- getQ :: Q (Maybe FuncNameTbl)
  let tbl = maybe S.empty fromFuncNameTbl mtbl
      found = S.member name tbl
      newTbl = S.insert name tbl
  putQ newTbl
  return found

tryReserveStruct :: StructBaseName -> Q Bool
tryReserveStruct name = do
  mtbl <- getQ :: Q (Maybe StructNameTbl)
  let tbl = maybe S.empty fromStructNameTbl mtbl
      found = S.member name tbl
      newTbl = S.insert name tbl
  putQ newTbl
  return found

--makeArray :: forall a. (HLCTypeable a) => SafeName -> TypedVar Int -> HLC_ (TypedVar 

makeVar :: forall a. (HLCTypeable a) => SafeName -> HLC (TypedVar a)
makeVar name = HLC $ do
  symb <- makeHLCSymbol_ name
  writeVar $ Variable symb (fromTW (hlcType :: TW a)) Nothing
  return $ TypedVar symb

assignVar :: (HLCTypeable a) => TypedLHS a -> TypedExpr a -> HLC ()
assignVar lhs rhs = HLC $ writeStmt $
  HLCAssignment (helper lhs) (fromTypedExpr rhs)
  where helper :: TypedLHS b -> UntypedLHS
        helper (TypedLHSVar v) = LHSVar $ fromTypedVar v
        helper (TypedLHSDeref t) = LHSDeref $ helper t
        helper (TypedLHSDerefPlusOffset t offset) = LHSDerefPlusOffset (helper t) offset
        helper (TypedLHSElement st name) = LHSElement (helper st) (getFieldName name)

declareFunction :: forall a. (HLCTypeable a) =>
                   String -> [Argument] -> HLC (TypedExpr a) -> HLC HLCSymbol
declareFunction baseName args (HLC body) = HLC $ do
  symb <- declareFuncInst (FuncBaseName baseName) $ map argumentType args
  let retType = fromTW (hlcType :: TW a)
  writeFuncProto $ FunctionProto retType symb args
  writeFunctionM symb args body
  return symb

getILTypeFromTHType :: Type -> Q Exp
getILTypeFromTHType t =
  appE (varE $ mkName "fromTW") $
  sigE (varE $ mkName "hlcType") $
  appT (conT $ mkName "TW") (return t)

getArgTypes :: Name -> Q [Exp]
getArgTypes fName = do
  (VarI _ t _ _) <- reify fName
  sequence $ recurse t
    where recurse (ForallT _ _ rest) = recurse rest
          recurse (AppT (AppT ArrowT t) rest) =
            getILTypeFromTHType t : recurse rest
          recurse _ = []

getFunHLCSymbol :: String -> [ILType] -> HLC (Maybe HLCSymbol)
getFunHLCSymbol baseName args = HLC $
  lookupFunc $ FunctionInst (FuncBaseName baseName) args

makeFunctionCall :: forall a. (HLCTypeable a) =>
                    String -> [ILType] -> HLC (TypedExpr a) -> [HLCExpr] -> HLC (TypedExpr a)
makeFunctionCall cFunNameStr argTypes f args = do
  let numArgs = length argTypes
  mSymb <- getFunHLCSymbol cFunNameStr argTypes
  symb <- case mSymb of
    Just s -> return s
    Nothing -> do
      let cArgNames = map (("arg" ++) . show) [1..numArgs]
      argSymbols <- mapM (makeHLCSymbol . makeSafeName) cArgNames
      declareFunction cFunNameStr (zipWith Argument argSymbols argTypes) f
  return $ TypedExpr $ FunctionCall (ExpVar symb) args
      

-- Takes a function f :: Args -> HLC (TypedExpr a) and create call_f :: Args -> HLC (TypedExpr a)
createFunction :: Name -> Q [Dec]
createFunction fname = do
  let cFunName = makeFuncBaseName $ nameBase fname
      cFunNameStr = litE $ stringL (fromFuncBaseName cFunName)
  nameReserved <- tryReserveFunc cFunName
  case nameReserved of
    True -> fail ("A function with this name has already been created: " ++ fromFuncBaseName cFunName)
    False -> return ()
  let newFuncNameStr = "call_" ++ nameBase fname
      newFuncName = mkName newFuncNameStr
  newNameExists <- lookupValueName newFuncNameStr
  case newNameExists of
    Nothing -> return ()
    Just _ -> fail ("Tried to generate haskell function " ++ newFuncNameStr ++ " but it already exists")
  argTypes <- getArgTypes fname
  argNames <- mapM newName $ map (const "x") argTypes
  let numArgs = litE $ integerL $ fromIntegral $ length argNames
      callToF = appsE (varE fname : map varE argNames)
      argTypeList = return $ ListE argTypes
      argExprs = listE $ map (appE (varE (mkName "fromTypedExpr")) . varE) argNames
  let body = [e| makeFunctionCall $cFunNameStr $argTypeList $callToF $argExprs |]
  retVal <- funD newFuncName [clause (map varP argNames) (normalB body) []]
  return [retVal]

readElt :: forall structType fieldName fieldType.
           (Struct structType fieldName fieldType, Typeable fieldName) =>
           TypedExpr structType ->
           Proxy fieldName ->
           TypedExpr fieldType
readElt struct _ =
  TypedExpr $ AccessPart (fromTypedExpr struct) $
  getFieldName (Proxy :: Proxy fieldName)


instance HLCTypeable Int where
  hlcType = TW (BaseType Unsigned NotConst ILInt)
  structDef = Nothing

instance HLCTypeable Char where
  hlcType = TW (BaseType Unsigned NotConst ILChar)
  structDef = Nothing



