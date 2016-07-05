{-# LANGUAGE DataKinds #-}

module Quasi.TypeParser where

import Language.Haskell.Exts
import Language.Haskell.Exts.Extension

import qualified Language.Haskell.TH as TH

parseMode = defaultParseMode {extensions = [EnableExtension TypeOperators,
                                            EnableExtension GADTs,
                                            EnableExtension DataKinds]}

parseSomeType str = case parseTypeWithMode parseMode str of
  ParseOk x -> x
  (ParseFailed _ err) -> error (show err)

promotedToTH :: Promoted -> TH.Type
promotedToTH (PromotedInteger n) = TH.LitT $ TH.NumTyLit n
promotedToTH (PromotedString str) = TH.LitT $ TH.StrTyLit str
promotedToTH (PromotedCon _ qname) = TH.PromotedT $ TH.mkName $ prettyPrint qname
promotedToTH (PromotedList _ (x:xs)) =
  TH.AppT (TH.AppT TH.PromotedConsT (extTypeToTHType x)) $
  promotedToTH (PromotedList undefined xs)
promotedToTH (PromotedList _ []) =
  TH.PromotedNilT
promotedToTH (PromotedTuple tys) =
  foldl TH.AppT (TH.PromotedTupleT (length tys)) $
  map extTypeToTHType tys
promotedToTH PromotedUnit = TH.PromotedTupleT 0

extTypeToTHType :: Type -> TH.Type
extTypeToTHType (TyFun arg result) = error "Function ptrs not yet implemented"
extTypeToTHType (TyTuple _ tys) =
  foldl TH.AppT (TH.TupleT (length tys)) $
  map extTypeToTHType tys
extTypeToTHType (TyInfix lhs qname rhs) =
  TH.AppT (TH.AppT (TH.ConT $ TH.mkName $ prettyPrint qname) (extTypeToTHType lhs))
  (extTypeToTHType rhs)
extTypeToTHType (TyKind ty kind) = extTypeToTHType ty
extTypeToTHType (TyPromoted prom) = promotedToTH prom
extTypeToTHType (TyBang b ty) = extTypeToTHType ty
extTypeToTHType (TyApp lhs rhs) = TH.AppT (extTypeToTHType lhs) (extTypeToTHType rhs)
extTypeToTHType (TyCon name) = TH.ConT (TH.mkName $ prettyPrint name)
extTypeToTHType (TyVar name) = TH.VarT (TH.mkName $ prettyPrint name)
extTypeToTHType (TyParen ty) = extTypeToTHType ty
extTypeToTHType (TyEquals lhs rhs) =
  TH.AppT (TH.AppT TH.EqualityT $ extTypeToTHType lhs) $ extTypeToTHType rhs

getName :: TyVarBind -> TH.TyVarBndr
getName (KindedVar name _) = TH.PlainTV $ TH.mkName $ prettyPrint name
getName (UnkindedVar name) = TH.PlainTV $ TH.mkName $ prettyPrint name

getTyParams :: Type -> [TH.TyVarBndr]
getTyParams (TyForall binds cxt ty)= maybe [] (map getName) binds ++ getTyParams ty
getTyParams (TyFun arg result) = getTyParams arg ++ getTyParams result
getTyParams (TyTuple _ tys) = concatMap getTyParams tys
getTyParams (TyList ty) = getTyParams ty
getTyParams (TyApp lhs rhs) = getTyParams lhs ++ getTyParams rhs
getTyParams (TyVar name) = [TH.PlainTV $ TH.mkName $ prettyPrint name]
getTyParams (TyCon qname) = []
getTyParams (TyParen ty) = getTyParams ty
getTyParams (TyInfix lhs op rhs) =
  (TH.PlainTV $ TH.mkName $ prettyPrint op) :
  (getTyParams lhs ++ getTyParams rhs)
getTyParams (TyKind ty _) = getTyParams ty
getTyParams (TyPromoted _) = []

asstToConstraint :: Asst -> TH.Type
asstToConstraint (ClassA className args) =
  extTypeToTHType (foldl TyApp (TyCon className) args)
asstToConstraint (AppA className args) =
  extTypeToTHType (foldl TyApp (TyCon $ UnQual className) args)
asstToConstraint (InfixA lhs className rhs) =
  extTypeToTHType (TyApp (TyApp (TyCon className) lhs) rhs)
asstToConstraint (IParam _ _) =
  error "Implicit params not supported"
asstToConstraint (EqualP lhs rhs) =
  extTypeToTHType (TyEquals lhs rhs)
asstToConstraint (ParenA asst) =
  asstToConstraint asst
asstToConstraint (WildCardA _) =
  error "WildCards not supported"


getConstraints :: Type -> [TH.Type]
getConstraints (TyForall _ assts _) = map asstToConstraint assts
getConstraints _ = []

getRetType :: Type -> TH.Type
getRetType (TyForall binds cxt ty)= getRetType ty
getRetType (TyFun arg result) = getRetType result
getRetType (TyTuple _ tys) =
  foldl TH.AppT (TH.TupleT $ length tys) $
  map extTypeToTHType tys
getRetType (TyList ty) = extTypeToTHType ty
getRetType (TyApp lhs rhs) = extTypeToTHType (TyApp lhs rhs)
getRetType (TyVar name) = extTypeToTHType (TyVar name)
getRetType (TyCon qname) = extTypeToTHType (TyCon qname)
getRetType (TyParen ty) = extTypeToTHType (TyParen ty)
getRetType (TyInfix lhs op rhs) =
  extTypeToTHType (TyInfix lhs op rhs)
getRetType (TyKind ty _) = getRetType ty
getRetType (TyPromoted ty) = promotedTypeToTHType ty

getFuncComponents :: Type -> [TH.Type]
getFuncComponents (TyForall binds cxt ty)= getFuncComponents ty
getFuncComponents (TyFun arg result) =
  extTypeToTHType arg :
  getFuncComponents result
getFuncComponents (TyTuple _ tys) =
  [foldl TH.AppT (TH.TupleT $ length tys) $
   map extTypeToTHType tys]
getFuncComponents (TyList ty) = [extTypeToTHType ty]
getFuncComponents (TyApp lhs rhs) = [extTypeToTHType (TyApp lhs rhs)]
getFuncComponents (TyVar name) = [extTypeToTHType (TyVar name)]
getFuncComponents (TyCon qname) = [extTypeToTHType (TyCon qname)]
getFuncComponents (TyParen ty) = [extTypeToTHType (TyParen ty)]
getFuncComponents (TyInfix lhs op rhs) =
  [extTypeToTHType (TyInfix lhs op rhs)]
getFuncComponents (TyKind ty _) = getFuncComponents ty
getFuncComponents (TyPromoted promotedType) = [promotedTypeToTHType promotedType]

promotedTypeToTHType :: Promoted -> TH.Type
promotedTypeToTHType (PromotedInteger n) = TH.LitT (TH.NumTyLit n)
promotedTypeToTHType (PromotedString str) = TH.LitT (TH.StrTyLit str)
promotedTypeToTHType (PromotedCon _ name) = TH.PromotedT (TH.mkName $ prettyPrint name)
promotedTypeToTHType (PromotedList _ []) =
  TH.ListT
promotedTypeToTHType (PromotedList _ (x:xs)) =
  TH.AppT (TH.AppT TH.PromotedConsT (extTypeToTHType x)) $
  promotedTypeToTHType (PromotedList True xs)
promotedTypeToTHType (PromotedTuple args) =
  foldl (\acc elt -> TH.AppT elt acc) (TH.PromotedTupleT (length args)) $
  map extTypeToTHType args
promotedTypeToTHType PromotedUnit = TH.PromotedTupleT 0
