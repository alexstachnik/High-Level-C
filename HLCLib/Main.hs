{-# OPTIONS_GHC -fwarn-unused-do-bind #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Language.C.Data.InputStream
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Data.Node
import Language.C.Data.Name
import Language.C.Parser
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants

import Debug.Trace
import Data.Proxy
import Data.Typeable

import Util.Names
import Quasi.Parser
import Quasi.QuasiC
import Quasi.QuasiTypes
import HighLevelC.HLC
import HighLevelC.HLCCalls
import HighLevelC.HLCTypes
import HighLevelC.CWriter
import HighLevelC.BasicTypes
import HighLevelC.PrimFunctions
import HighLevelC.VarDecls
import HighLevelC.Operators
import HighLevelC.TypeSynonyms
import HighLevelC.LangConstructs
import IntermediateLang.ILTypes
import Printer.Printer
import Printer.PostProcess

import Language.Haskell.TH
import Language.Haskell.TH as TH



$(generateFunction [funcDefn|test HLCVoid|])
test ret = do
  str <- stringLit "Hello, world!\n"
  callPrintf str NilArg
  ret void

$(generateFunction [funcDefn|fact HLCInt -> HLCInt|])

fact ret n = do
  ifThenElse (n %<= intLit 1)
    (ret n)
    (ret $ (n %* (call_fact (n %- intLit 1))))

$(generateFunction [funcDefn|hlcMain HLCInt -> HLCWeakPtr (HLCWeakPtr HLCChar) -> HLCInt|])
hlcMain ret argc argv = do
  exprStmt $ call_test
  ret (call_fact (intLit 3))

main :: IO ()
main = print $ printWholeTU (Just 'hlcMain) $ runOuterHLC $ do
  _ <- call_test
  _ <- call_fact (withType :: Type_Int)
  _ <- call_hlcMain (withType :: Type_Int) (withType :: MkWeakPtr (MkWeakPtr Type_Char))
  return ()
  

