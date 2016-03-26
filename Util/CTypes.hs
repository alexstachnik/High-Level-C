
module Util.CTypes where

import Language.C.Syntax.AST

data CBaseType = CVoid
               | CChar
               | CShort
               | CInt
               | CLong
               | CFloat
               | CDouble
               deriving (Eq,Ord,Show)

data TypeSign = Signed | Unsigned | NoSign
              deriving (Eq,Ord,Show)

data Constness = Const | NotConst
               deriving (Eq,Ord,Show)

data CValType = DirectType TypeSign Constness CBaseType
              | PtrType TypeSign Constness CValType
              deriving (Eq,Ord,Show)

data CFullType = FullValueType CValType
               | FuncPtrType CFullType [CFullType]
               | StructType [CFullType]
               | UnionType [CFullType]
               deriving (Eq,Ord,Show)

getTypeSpec :: [CDeclarationSpecifier a] -> CTypeSpecifier a
getTypeSpec [] = error "No type specifier in declaration"
getTypeSpec ((CTypeSpec x):_) = x
getTypeSpec (_:xs) = getTypeSpec xs

getConstness :: [CDeclarationSpecifier a] -> Constness
getConstness [] = NotConst
getConstness (CTypeQual (CConstQual _):xs) =  Const
getConstness (_:xs) = getConstness xs

typeofDecl :: CDeclaration a -> CFullType
typeofDecl (CDecl specs declrs _) =
  let const = getConstness 
      derived = mapMaybe (\(CDeclr _ d _ _ _) -> d) declrs in
  case mCdeclr of
    (Just (_, derivs, _, _)
