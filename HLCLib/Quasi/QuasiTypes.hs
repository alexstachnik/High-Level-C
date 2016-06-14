{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Quasi.QuasiTypes where

import Data.Word
import Data.Char
import Data.Data
import Data.Typeable
import HighLevelC.HLCTypes
import HighLevelC.BasicTypes
import Language.Haskell.TH

import Text.PrettyPrint

data Field = Field Name Type
           deriving (Eq,Ord,Show,Data)

data Function = Function {funcTyParams :: [TyVarBndr],
                          funcTyConstraints :: [Type],
                          retType :: Type,
                          funcArgTypes :: [Type],
                          funcBody :: Name,
                          funcCallName :: Name
                         }
                deriving (Eq,Ord,Show,Data)
                         
data StructDesc = StructDesc {structName :: Name,
                              structTyParams :: [TyVarBndr],
                              structTyConstraints :: [Type],
                              fields :: [Field],
                              isPassable :: Bool,
                              constructor :: Name,
                              destructor :: Name}
            deriving (Eq,Ord,Show,Data)


generateFunction :: Function -> Q [Dec]
generateFunction func = do
  funcName <- newName $ (\(x:xs) -> toUpper x : xs) $ (nameBase $ funcCallName func)
  generateFunction' funcName func

generateFunction' :: Name -> Function -> Q [Dec]
generateFunction' funcName (Function {..}) =
  sequence
  ([dataD (return []) funcName funcTyParams [] [],
    instanceD argConstraints (appT
                              (appT (appT [t|HLCFunction|] appliedFunc)
                               argWrap) (return retType))
    [return $ FunD (mkName "call")
     [Clause [WildP, (VarP $ mkName "retArg")]
      (NormalB (AppE argWrapConsE (AppE (VarE funcBody) (VarE $ mkName "retArg"))))
      []]],
    sigD funcCallName
    (forallT funcTyParams argAndPassConstraints funcCallType),
    funD funcCallName
    [clause [] (normalB (appE callExpr (sigE [e|Proxy|]
                                        [t| Proxy $appliedFunc |]))) []]
    ])

  where
    callExpr = return $ VarE $ mkName ("call" ++ (show $ length funcArgTypes))
    
    funcCallType :: Q Type
    funcCallType =
      foldr appT retHLC argArrows

    argArrows :: [TypeQ]
    argArrows = map (\var -> appT arrowT [t|HLC (TypedExpr $var)|]) (map return funcArgTypes)

    retHLC :: TypeQ
    retHLC = [t|HLC (TypedExpr $(return retType))|]
    
    argConstraints :: Q [Type]
    argConstraints =
      let hlcTyConstraints = map (AppT (ConT $ mkName "HLCTypeable")) tyVars in
      return (hlcTyConstraints ++ funcTyConstraints)

    passConstraint :: Type -> Q Type
    passConstraint tyvar = [t|Passability $(return tyvar) ~ IsPassable|]

    argAndPassConstraints = do
      c <- argConstraints
      passConstraints <- mapM passConstraint tyVars
      return (c ++ passConstraints)
      
    unappliedFunc = ConT funcName
    
    appliedFunc = return $ foldl AppT unappliedFunc tyVars

    argWrapConsE = ConE $ mkName ("ArgWrap" ++ (show $ length funcArgTypes))

    argWrapCons = ConT $ mkName ("ArgWrap" ++ (show $ length funcArgTypes))
    
    argWrap = do
      typedExpr <- [t|TypedExpr|]
      return $ foldl AppT argWrapCons (map (AppT typedExpr) funcArgTypes)
    
    tyVars = map getTyVar funcTyParams

getTyVar :: TyVarBndr -> Type
getTyVar (PlainTV name) = VarT name
getTyVar (KindedTV name _) = VarT name

makeStructFieldType structPassability appliedData =
  [t|forall fieldName fieldType.
   StructFieldClass $(structPassability) $(appliedData) fieldName fieldType =>
   Proxy fieldName -> HLC (TypedLHS fieldType)|]


generateStructDesc :: StructDesc -> Q [Dec]
generateStructDesc (StructDesc {..}) =
  sequence
  ([dataD (return []) structName structTyParams [] [],
    standaloneDerivD typeableConstraints (appT [t|Typeable|] appliedData),
    instanceD structConstraints (appT [t|HLCTypeable|] appliedData)
    [return $ ValD (VarP $ mkName "hlcType") (NormalB (VarE $ mkName "structHLCType")) []]] ++
   map fieldData fields ++
   map makeStructField fields ++
   [instanceD structConstraints (appT (appT [t|Struct|] structPassability) appliedData)
    [return $ ValD (VarP $ mkName "constructor") (NormalB (VarE constructor)) [],
     return $ ValD (VarP $ mkName "destructor") (NormalB (VarE destructor)) []],
    sigD constructor (forallT structTyParams structConstraints consType)])
  where
    consType =
      (appT (appT arrowT (appT [t|Proxy|] appliedData))
       (appT (appT arrowT (makeStructFieldType structPassability appliedData))
              (appT (appT arrowT [t|Context|]) [t|HLC Context|])))
    makeStructField (Field name ty) =
      instanceD structConstraints
      (appT
       (appT
        (appT
         (appT [t|StructFieldClass|] structPassability)
         appliedData)
        (conT name))
       (return ty))
      []

    structPassability = case isPassable of
      True -> [t|IsPassable|]
      False -> [t|NotPassable|]
       
    fieldData :: Field -> DecQ
    fieldData (Field name ty) = dataD (return []) name [] [] [''Typeable]
    
    passConstraint :: Type -> Q Type
    passConstraint tyvar = [t|Passability $(return tyvar) ~ IsPassable|]
    
    structConstraints :: Q [Type]
    structConstraints = do
      hlcTyConstraints <- mapM (appT [t|HLCTypeable|] . return) tyVars
      passConstraints <- case isPassable of
        True -> mapM passConstraint tyVars
        False -> return []
      return (hlcTyConstraints ++ passConstraints ++ structTyConstraints)
    
    typeableConstraints :: Q [Type]
    typeableConstraints = mapM (appT [t|Typeable|] . return) tyVars
    
    unappliedData :: Type
    unappliedData = ConT structName

    tyVars :: [Type]
    tyVars = map getTyVar structTyParams

    appliedData :: Q Type
    appliedData = return $ foldl AppT unappliedData tyVars

