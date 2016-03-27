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

import Util.CTypes

main :: IO ()
main = print 3

f = inputStreamFromString "x=a-b;"

g = execParser_ statementP f nopos

--x = let hey = internalIdent "boom" in [cExpr| f(3)|]


z = let x info =  (CConst (CIntConst (cInteger 3) info)) in
  [cExpr|3+$x|]


c = [cDecl|char f(int y, int z) {x=3;}|]
  
d = [cStmt|{void (*f)(struct foo,unsigned char);}|]


e = [cStmt|{void (*f())();}|]

q = [cStmt|{typedef int foo; foo y;} |]


