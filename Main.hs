{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}

module Main(main) where



import Language.C.Data.InputStream
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Parser
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants

import Data.Typeable

import Util.Names
import Quasi.QuasiC
import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.CWriter
import HighLevelC.BasicTypes
import IntermediateLang.ILTypes

main :: IO ()
main = print 3


data MyStruct deriving (Typeable)

data FieldA deriving (Typeable)
data FieldB deriving (Typeable)

instance Struct MyStruct FieldA HLCInt
instance Struct MyStruct FieldB HLCChar



instance HLCTypeable MyStruct where
  structDef = Just $ makeStructDef
    [makeStructField (Proxy :: Proxy FieldA) Nothing,
     makeStructField (Proxy :: Proxy FieldB) Nothing]


f_p = Function f (FuncBaseName "ff")
  where f a = do
          test <- makeVar (makeSafeName "test") :: HLC (TypedVar MyStruct)
          test2 <- makeVar (makeSafeName "test2") :: HLC (TypedVar HLCInt)
          assignVar (TypedLHSVar test2) (makeIntLit 3)
          return makeVoid



g = do
  test <- makeInt "blah"
  _ <- call1 f_p (varRef test)
  return ()


--q :: Function (TypedExpr HLCChar -> (HLC (TypedExpr HLCInt)))
--q = ExtFunc (ExtSymbol "foo")

q = [cExpr|sizeof(int)|]
