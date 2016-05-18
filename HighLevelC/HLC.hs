{-# LANGUAGE FlexibleContexts #-}
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

import Debug.Trace

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import HighLevelC.HLCTypes
import HighLevelC.CWriter
import Util.THUtil
import Util.Names
import IntermediateLang.ILTypes


newtype FuncNameTbl = FuncNameTbl {fromFuncNameTbl :: S.Set FuncBaseName}
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

lookupStruct :: StructDef -> HLC_ Bool
lookupStruct x = gets structInstances >>= (return . S.member x. fromStructInstances)

addFuncInst :: FunctionInst -> HLCSymbol -> HLC_ ()
addFuncInst inst symb = modify $ \(CodeState {..}) ->
  CodeState {funcInstances = FuncInstances $ M.insert inst symb $ fromFuncInstances funcInstances,
             ..}

addStructInst :: StructDef -> HLC_ ()
addStructInst inst = modify $ \(CodeState {..}) ->
  CodeState {structInstances = StructInstances $ S.insert inst $ fromStructInstances structInstances,
             ..}

declareFuncInst :: FuncBaseName -> [ILType] -> HLC_ HLCSymbol
declareFuncInst baseName args = do
  let newName = deriveFuncName baseName args
  symb <- makeHLCSymbol_ newName
  addFuncInst (FunctionInst baseName args) symb
  return symb

declareStruct :: StructDef -> HLC_ ()
declareStruct struct = do
  exists <- lookupStruct struct
  case exists of
    True -> return ()
    False -> do
      addStructInst struct
      writeStruct struct

makeVar :: forall a. (HLCTypeable a) => SafeName -> HLC (TypedVar a)
makeVar name = HLC $ do
  let decl = structDef :: Maybe (TypedStructDef a)
  case decl of
    Just (TypedStructDef def) -> declareStruct def
    Nothing -> return ()
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
                   FuncBaseName -> [Argument] -> HLC (TypedExpr a) -> HLC HLCSymbol
declareFunction baseName args (HLC body) = HLC $ do
  symb <- declareFuncInst baseName $ map argumentType args
  let retType = fromTW (hlcType :: TW a)
  writeFuncProto $ FunctionProto retType symb args
  writeFunctionM symb args body
  return symb

getFunHLCSymbol :: FuncBaseName -> [ILType] -> HLC (Maybe HLCSymbol)
getFunHLCSymbol baseName args = HLC $
  lookupFunc $ FunctionInst baseName args

readElt :: forall structType fieldName fieldType.
           (Struct structType fieldName fieldType, Typeable fieldName) =>
           TypedExpr structType ->
           Proxy fieldName ->
           TypedExpr fieldType
readElt struct _ =
  TypedExpr $ AccessPart (fromTypedExpr struct) $
  getFieldName (Proxy :: Proxy fieldName)


makeStructField :: forall structType fieldName fieldType.
                   (Struct structType fieldName fieldType,
                    Typeable structType,
                    Typeable fieldName,
                    HLCTypeable fieldType) =>
                   Proxy fieldName ->
                   Maybe Integer ->
                   TypedStructField structType
makeStructField _ mArrLen =
  TypedStructField $
  StructField
  (getFieldName (Proxy :: Proxy fieldName))
  (fromTW (hlcType :: TW fieldType))
  mArrLen

makeStructDef :: forall structType.
                 (Typeable structType) =>
                 [TypedStructField structType] ->
                 TypedStructDef structType
makeStructDef =
  TypedStructDef .
  StructDef (getILType (Proxy :: Proxy structType)) . map fromTypedStructField
  
data Function a = Function a FuncBaseName
                | ExtFunc HLCSymbol

newtype VarFunction a = VarFunction HLCSymbol

callVarFunc :: (HLCTypeable b) =>
               VarFunction (HLC (TypedExpr b)) ->
               VarArg ->
               HLC (TypedExpr b)
callVarFunc (VarFunction symb) varArgs =
  return $ TypedExpr $ FunctionCall (ExpVar symb) $ varArgToList varArgs

makeCallOperator :: Int -> DecsQ
makeCallOperator n = do
  let fName = mkName ("call" ++ show n)
      fTypeArgs = map (\s -> mkName ("a" ++ show s)) [1..n]
  retTypeArg <- newName "b"
  let tyVars = map PlainTV (retTypeArg : fTypeArgs)
      tyConstraints =
        mapM (\argVar -> appT [t|HLCTypeable|] (varT argVar))
        (retTypeArg : fTypeArgs)
      fRetType = appT [t|HLC|] (appT [t|TypedExpr|] $ varT retTypeArg)
      fArgType = foldr (\arg acc -> appT (appT arrowT arg) acc) fRetType $
        map (\var -> appT [t|TypedExpr|] (varT var)) fTypeArgs
      functionArg = appT [t|Function|] fArgType
  sig <- sigD fName $
    forallT tyVars tyConstraints (appT (appT arrowT functionArg) fArgType)
  fCallArg <- newName "f"
  fBaseName <- newName "baseName"
  fArgs <- mapM (const $ newName "c") [1..n]
  fArgSymbols <- mapM (const $ newName "d") [1..n]
  let normalFunctionClause =
        [[p|Function $(varP fCallArg) $(varP fBaseName)|]] ++
        map varP fArgs
      typeArgs =
        map (\arg -> appE [e|getObjType|] arg) $
        map varE fArgs
      args = listE $ zipWith (\eArg tArg -> appE (appE [e|Argument|] (varE eArg))
                                            (appE [e|getObjType|] (varE tArg)))
        fArgSymbols fArgs
      makeArgSymbols = map (\arg -> bindS (varP arg) $
                                    appE [e|makeHLCSymbol|] $
                                    appE [e|makeSafeName|] $
                                    litE (stringL $ show arg)) fArgSymbols
      fCall = appsE (varE fCallArg :
                     map (\arg -> appE [e|TypedExpr|] (appE [e|ExpVar|] (varE arg))) fArgSymbols)
      makeFunDecl = makeArgSymbols ++
        [noBindS $ appsE [[e|declareFunction|],varE fBaseName,args,fCall]]
      normalBody = normalB $ doE
        [bindS (varP $ mkName "mFSymb") $ appsE [[e|getFunHLCSymbol|],(varE fBaseName),listE typeArgs],
         bindS (varP $ mkName "funcName") $ caseE (varE $ mkName "mFSymb") [
            match (conP (mkName "Just") [varP $ mkName "x"]) (normalB $ appE [e|return|] (varE $ mkName "x")) [],
            match (conP (mkName "Nothing") []) (normalB $ doE makeFunDecl) []],
         noBindS $ appE [e|return|] $ appE [e|TypedExpr|] $ appE (appE [e|FunctionCall|] (appE [e|ExpVar|] (varE $ mkName "funcName"))) $
         listE $ map (appE [e|fromTypedExpr|] . varE) fArgs]
      extFunctionClause = [[p|ExtFunc $(varP fCallArg)|]] ++ map varP fArgs
      extBody = normalB $ appE [e|return|] $
        appE [e|TypedExpr|] $
        appE (appE [e|FunctionCall|] (appE [e|ExpVar|] (varE fCallArg))) $
        listE $ map (appE [e|fromTypedExpr|] . varE) fArgs
  fDecl <- funD fName [clause normalFunctionClause normalBody [],
                       clause extFunctionClause extBody []]
  return [sig,fDecl]

call2 :: forall b a1 a2 name.
         (HLCTypeable a1,
          HLCTypeable a2,
          HLCTypeable b,
          HLCFunction name (TypedExpr a1 ->
                            TypedExpr a2 ->
                            HLC (TypedExpr b))) =>
         Proxy name ->
         TypedExpr a1->
         TypedExpr a2 ->
         HLC (TypedExpr b)
call2 name a1 a2 = do
  let baseName = FuncBaseName $ fromSafeName $ getFieldName name
  mFSymb <- getFunHLCSymbol baseName [getObjType a1,
                                      getObjType a2]
  funcName <- case mFSymb of
    Just x -> return x
    Nothing -> do
      d1 <- makeHLCSymbol (makeSafeName "d1")
      d2 <- makeHLCSymbol (makeSafeName "d2")
      declareFunction baseName [Argument d1 (getObjType a1),
                                Argument d2 (getObjType a2)]
        ((call name) (TypedExpr (ExpVar d1)) (TypedExpr (ExpVar d2)))
  return (TypedExpr (FunctionCall (ExpVar funcName) [fromTypedExpr a1, fromTypedExpr a2]))
