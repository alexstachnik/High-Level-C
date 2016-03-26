{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monads.Code(Code) where

import Data.Sequence

import Control.Monad.State(StateT,evalStateT,MonadState,gets,modify)
import Control.Monad.Writer(WriterT,execWriterT,MonadWriter)
import Control.Monad.Identity(Identity(runIdentity))

import Language.C.Syntax.AST(CExtDecl)

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Maybe(mapMaybe,listToMaybe)

newtype VarName = VarName String
                  deriving (Eq,Ord,Show)



data Signature = Signature FuncName [CmpCTypeSpec]
                    deriving (Eq,Ord,Show)


data Variable = Variable VarName CmpCTypeSpec
                deriving (Eq,Ord,Show)

data CodeState = CodeState {instances :: S.Set Signature,
                            scope :: [M.Map VarName CmpCTypeSpec]}
                 deriving (Show)

data Function retVal = Function FuncName retVal

data Argument = Argument VarName CmpCTypeSpec
                deriving (Eq,Ord,Show)

newtype Code a = Code {
  runCode::(WriterT (Seq CExtDecl)
            (StateT CodeState Identity) a)}
               deriving (Monad,
                         Applicative,
                         Functor,
                         MonadState CodeState,
                         MonadWriter (Seq CExtDecl))


initState = CodeState {instances = S.empty,
                       scope = []}

runCode_ :: Code () -> Seq CExtDecl
runCode_ c =
  runIdentity $ evalStateT (execWriterT $ runCode c) initState
  
lookupVar :: VarName -> Code (Maybe CmpCTypeSpec)
lookupVar name = do
  curScope <- gets scope
  return $ listToMaybe $ mapMaybe (M.lookup name) curScope

addToScope :: Variable -> Code ()
addToScope (Variable name ctype) = modify $ \(CodeState {..}) ->
  CodeState {scope = M.insert name ctype (head scope) : tail scope,..}

pushScope :: Code ()
pushScope = modify $ \(CodeState {..}) ->
  CodeState {scope = M.empty : scope,..}

popScope :: Code ()
popScope = modify $ \(CodeState {..}) ->
  CodeState {scope = tail scope,..}

typeToReturnType :: CmpCTypeSpec -> [CmpCDeclSpec]

makeFunction :: [CDeclSpec] -> FuncName -> [Argument] -> Code CStat -> Code (CFunctionDef ())
makeFunction retType fname args bodyM = do
  pushScope
  addVarsToScope args
  body <- bodyM
  popScope
  case S.member sig curInstances of
    True -> return ()
    False -> writeFunction $
  CFunDef retType (makePrototype fname args) [] body

--x :: Function (Int -> RingElt -> FunctionCall RingElt)

--makeReturn :: Function -> Signature -> a -> Code (FunctionCall a)
--makeReturn sig retVal = do
--  curInstances <- gets instances
--case S.member sig curInstance of
--    True -> return ()
--    False -> writeFunction
