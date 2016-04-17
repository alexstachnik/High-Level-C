{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main(main) where



import Language.C.Data.InputStream
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Parser
import Language.C.Pretty
import Language.C.Syntax.AST
import Language.C.Syntax.Constants

import Quasi.QuasiC
import HighLevelC.HLC
import HighLevelC.HLCTypes
import HighLevelC.CWriter

main :: IO ()
main = print 3


makeCharLit :: Char -> TypedExpr Char
makeCharLit c = TypedExpr $ CharLit c

f :: TypedExpr Int -> TypedExpr Int -> HLC (TypedExpr Char)
f a b = return $ makeCharLit 'c'

createFunction 'f

c = [cDecl|char f(int y, int z) {test1: x=3; test2: x=4;}|]
