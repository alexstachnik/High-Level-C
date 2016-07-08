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
import Util.Names
import Language.Haskell.TH

import Text.PrettyPrint

data Field = Field Name Type
           deriving (Eq,Ord,Show,Data)

data ExtField = ExtField Name Name Type
           deriving (Eq,Ord,Show,Data)

data Function = Function {funcTyParams :: [TyVarBndr],
                          funcTyConstraints :: [Type],
                          retType :: Type,
                          funcArgTypes :: [Type],
                          funcBody :: Name
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
  funcName <- newName $ (\(x:xs) -> toUpper x : xs) $ (nameBase $ funcBody func)
  let funcCallName = mkName $ ("call_" ++) $ (nameBase $ funcBody func)
  generateFunction' funcName funcCallName func

generateFunction' :: Name -> Name -> Function -> Q [Dec]
generateFunction' funcName funcCallName (Function {..}) =
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
    (forallT funcTyParams argAndPassConstraints appliedFuncTyWrap),
    funD funcCallName
    [clause [] (normalB (appE (varE funcWrap)
                         (appE callExpr (sigE [e|Proxy|]
                                         [t| Proxy $appliedFunc |])))) []]
    ])

  where
    appliedFuncTyWrap = return $ foldl AppT (ConT funcTyWrap) (funcArgTypes ++ [retType])
    funcTyWrap = mkName ("FuncTyWrap" ++ (show $ length funcArgTypes))
    funcWrap = mkName ("funcWrap" ++ (show $ length funcArgTypes))
    
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
    
    argWrap =
      foldl appT (return argWrapCons) $
      map (appT [t|TypedLHS|]) $ map return funcArgTypes

    
    tyVars = map getTyVar funcTyParams

getTyVar :: TyVarBndr -> Type
getTyVar (PlainTV name) = VarT name
getTyVar (KindedTV name _) = VarT name

makeStructFieldType appliedData =
  [t|forall fieldName fieldType.
   StructFieldClass $(appliedData) fieldName fieldType =>
   Proxy fieldName -> HLC (TypedLHS fieldType)|]

makeFieldName :: Field -> Q ExtField
makeFieldName (Field name ty) = do
  name' <- newName $ (\(x:xs) -> toUpper x : xs) $ (nameBase name)
  return $ ExtField name' name ty

generateStructDesc :: StructDesc -> Q [Dec]
generateStructDesc structDesc = do
  extFields <- mapM makeFieldName $ fields structDesc
  generateStructDesc' extFields structDesc

makeFieldListStructField :: ExtField -> TypeQ
makeFieldListStructField (ExtField name _ ty) =
  appT (appT (promotedTupleT 2) (conT name)) (return ty)

promotedListT :: [TypeQ] -> TypeQ
promotedListT (x:xs) =
  appT (appT promotedConsT x) $
  promotedListT xs
promotedListT [] = promotedNilT

generateStructDesc' :: [ExtField] -> StructDesc -> Q [Dec]
generateStructDesc' extFields (StructDesc {..}) =
  sequence
  ([dataD (return []) structName structTyParams [] [],
    standaloneDerivD typeableConstraints (appT [t|Typeable|] appliedData),
    instanceD structConstraints (appT [t|HLCTypeable|] appliedData)
    [return $ ValD (VarP $ mkName "hlcType") (NormalB (VarE $ mkName "structHLCType")) []]] ++
   map fieldData extFields ++
   [instanceD structConstraints (appT [t|Struct|] appliedData)
    [tySynInstD (mkName "StructPassability") $ tySynEqn [appliedData] structPassability,
     tySynInstD (mkName "StructFields") $ tySynEqn [appliedData] structFieldTyList,
     return $ ValD (VarP $ mkName "constructor") (NormalB (VarE constructor)) [],
     return $ ValD (VarP $ mkName "destructor") (NormalB (VarE destructor)) []],
    sigD constructor (forallT structTyParams structConstraints consType)] ++
   map makeAccessor extFields
  )
  where
    structFieldTyList :: TypeQ
    structFieldTyList = promotedListT $ map makeFieldListStructField extFields
      
    structTypeName = mkName ("Type_" ++ nameBase structName)
    
    structProxyBody =
      normalB (sigE [e|Proxy|] $ forallT structTyParams (return []) (appT [t|Proxy|] appliedData))
      
    proxyTyVars = map (appT arrowT) $ map (appT [t|Proxy|]) $ map return tyVars
    
    structProxyName = mkName ("type_" ++ nameBase structName)
    
    makeAccessor (ExtField fieldName accName ty) =
      valD (varP accName) (normalB (sigE [e|Proxy|] (appT [t|Proxy|] (conT fieldName)))) []

    proxyRetTy = appT [t|Proxy|] appliedData
    
    consType =
      (appT (appT arrowT (appT [t|Proxy|] appliedData))
       (appT (appT arrowT (appT [t|HLC|] (appT [t|TypedLHS|] appliedData)))
              (appT (appT arrowT [t|HLC Context|]) [t|HLC Context|])))
    
    makeStructField (ExtField fieldName accName ty) =
      instanceD structConstraints
      (appT
       (appT
        (appT
         [t|StructFieldClass|]
         appliedData)
        (conT fieldName))
       (return ty))
      []

    structPassability = case isPassable of
      True -> [t|IsPassable|]
      False -> [t|NotPassable|]
       
    fieldData :: ExtField -> DecQ
    fieldData (ExtField fieldName accName ty) = dataD (return []) fieldName [] [] [''Typeable]
    
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

