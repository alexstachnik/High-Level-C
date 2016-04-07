{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module HighLevelC.HLC where

import Control.Monad.State(StateT,evalStateT,MonadState,gets,modify)
import Control.Monad.Writer(WriterT,execWriterT,MonadWriter,tell)
import Control.Monad.Identity(Identity(runIdentity))

import Language.C.Syntax.AST(CExtDecl)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq

import Data.Maybe(mapMaybe,listToMaybe)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import HighLevelC.HLCTypes
import HighLevelC.CWriter
import HighLevelC.Scope
import IntermediateLang.ILTypes

data CodeState = CodeState {definedFunctions :: S.Set Symbol,
                            globalScope :: Scope,
                            localScope :: Scope}
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

runInnerHLC :: HLC_ () -> CWriter
runInnerHLC c =
  runIdentity $ evalStateT (execWriterT $ runHLC_ c) initState

isFunctionDeclared :: Symbol -> HLC_ Bool
isFunctionDeclared x = do
  funcs <- gets definedFunctions
  return $ S.member x funcs

saveContext :: HLC_ Scope
saveContext = gets localScope

newContext :: HLC_ ()
newContext =
  modify $
  \(CodeState {..}) -> CodeState {localScope = globalScope,..}

restoreContext :: Scope -> HLC_ ()
restoreContext cxt =
  modify $
  \(CodeState {..}) -> CodeState {localScope = cxt,..}

declareFunction :: (HLCTypeable a) => Symbol -> HLC_ a -> HLC_ a
declareFunction name body = do
  oldCxt <- saveContext
  newContext
  body
  restoreContext oldCxt
  addToGlobalScope name
  return makeElt

tryReserveName :: Symbol -> Q Bool
tryReserveName name = do
  mtbl <- getQ :: Q (Maybe SymbolTable)
  let tbl = maybe emptySymbolTable id mtbl
      (found,newTbl) = addSymbolToTable name tbl
  putQ newTbl
  return found

createFunction :: Name -> [String] -> Q [Dec]
createFunction fname args = do
  let cFunName = Symbol $ nameBase fname
  nameReserved <- tryReserveName cFunName
  case nameReserved of
    True -> fail ("A function with this name has already been created: " ++ nameBase fname)
    False -> return ()
  let newFuncNameStr = "call_" ++ nameBase fname
  newNameExists <- lookupValueName newFuncNameStr
  case newNameExists of
    Nothing -> return ()
    Just x -> fail ("A function with this name already exists: " ++ newFuncNameStr)
  let newFuncName = mkName ("call_" ++ nameBase fname)
      argNames = map mkName args
      getTypeFromArg arg = appE [e|getObjType|] (varE arg)
      typeList = listE $ map getTypeFromArg argNames
      fnameExpr = stringE (nameBase fname)
      callToF = appsE (varE fname : map varE argNames)
      body = [e|
              let types = $typeList
                  baseFuncName = Symbol $fnameExpr
                  funcName = makeDerivedSymbol baseFuncName types in
              do
                done <- isFunctionDeclared funcName
                case done of
                  True -> return makeElt
                  False -> declareFunction funcName $callToF |]
  retVal <- funD newFuncName [clause (map varP argNames) (normalB body) []]
  return [retVal]


instance HLCTypeable Int where
  hlcType = TW (BaseType Unsigned NotConst ILInt)
  makeElt = 4


initState = CodeState {definedFunctions = S.empty,
                       globalScope = emptyScope,
                       localScope = emptyScope}
