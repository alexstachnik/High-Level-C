{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LanguageCStuff where



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

import Data.Typeable

import Util.Names
import Quasi.QuasiC
import HighLevelC.HLC
import HighLevelC.HLCCalls
import HighLevelC.HLCTypes
import HighLevelC.CWriter
import HighLevelC.BasicTypes
import HighLevelC.PrimFunctions
import HighLevelC.VarDecls
import IntermediateLang.ILTypes

import GHC.Generics
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

instance Out NodeInfo where
  docPrec _ _ = text "NodeInfo"
  doc _ = text "NodeInfo"

deriving instance Generic Name
deriving instance Generic NodeInfo
deriving instance Generic Ident
deriving instance Generic CFloat
deriving instance Generic CString
deriving instance Generic CChar
deriving instance Generic CInteger
deriving instance Generic (CTranslationUnit a)
deriving instance Generic (CExternalDeclaration a)
deriving instance Generic (CFunctionDef a)
deriving instance Generic (CDeclaration a)
deriving instance Generic CStructTag
deriving instance Generic (CStructureUnion a)
deriving instance Generic (CEnumeration a)
deriving instance Generic (CDeclarationSpecifier a)
deriving instance Generic (CStorageSpecifier a)
deriving instance Generic (CTypeSpecifier a)
deriving instance Generic (CTypeQualifier a)
deriving instance Generic (CAttribute a)
deriving instance Generic (CDeclarator a)
deriving instance Generic (CDerivedDeclarator a)
deriving instance Generic (CArraySize a)
deriving instance Generic (CInitializer a)
deriving instance Generic (CPartDesignator a)
deriving instance Generic (CStatement a)
deriving instance Generic (CCompoundBlockItem a)
deriving instance Generic (CAssemblyStatement a)
deriving instance Generic (CAssemblyOperand a)
deriving instance Generic (CExpression a)
deriving instance Generic CAssignOp
deriving instance Generic CBinaryOp
deriving instance Generic CUnaryOp
deriving instance Generic (CBuiltinThing a)
deriving instance Generic (CConstant a)
deriving instance Generic (CStringLiteral a)
deriving instance Generic (Flags a)
deriving instance Generic CIntRepr

instance Out Name
instance Out Ident
instance Out CFloat
instance Out CString
instance Out CChar
instance Out CInteger
instance Out (CTranslationUnit NodeInfo)
instance Out (CExternalDeclaration NodeInfo)
instance Out (CFunctionDef NodeInfo)
instance Out (CDeclaration NodeInfo)
instance Out CStructTag
instance Out (CStructureUnion NodeInfo)
instance Out (CEnumeration NodeInfo)
instance Out (CDeclarationSpecifier NodeInfo)
instance Out (CStorageSpecifier NodeInfo)
instance Out (CTypeSpecifier NodeInfo)
instance Out (CTypeQualifier NodeInfo)
instance Out (CAttribute NodeInfo)
instance Out (CDeclarator NodeInfo)
instance Out (CDerivedDeclarator NodeInfo)
instance Out (CArraySize NodeInfo)
instance Out (CInitializer NodeInfo)
instance Out (CPartDesignator NodeInfo)
instance Out (CStatement NodeInfo)
instance Out (CCompoundBlockItem NodeInfo)
instance Out (CAssemblyStatement NodeInfo)
instance Out (CAssemblyOperand NodeInfo)
instance Out (CExpression NodeInfo)
instance Out CAssignOp
instance Out CBinaryOp
instance Out CUnaryOp
instance Out (CBuiltinThing NodeInfo)
instance Out (CConstant NodeInfo)
instance Out (CStringLiteral NodeInfo)
instance Out (Flags a)
instance Out CIntRepr
--instance Out NodeInfo
--instance Out Name
--instance Out (CExternalDeclaration NodeInfo)
--instance Out (CTranslationUnit NodeInfo)



--q :: Function (TypedExpr HLCChar -> (HLC (TypedExpr HLCInt)))
--q = ExtFunc (ExtSymbol "foo")

q = [cExpr|sizeof(int)|]

qq = [cTransL|
struct MyStruct;
struct MyStruct {
  int x;
};
short*** f(int x[3]);
int main(int argc, char** argv);
int main(int argc, char** argv) {
  int x;
  return 0;
}|]

qpf = [cExpr|5<6|]


qp = [cTransL|
void f()
{
  int x;
  int y;
  int z;
  x=1;
  goto lab4;
  lab2:
  y=2;
  lab3:
  z=3;
  lab4:
  ;
}|]

qd = [cTransL|
void f()
{
  while (true) {
    f();
  }
}
|]

