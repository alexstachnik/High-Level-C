{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Quasi.QuasiC(cExpr,cStmt,cDecl,cTransL) where

import Data.List (span)
import Data.Maybe (isJust,catMaybes,listToMaybe)

import Data.Data (Data)

import Language.C.Parser(expressionP, statementP, extDeclP, translUnitP, execParser)
import Language.C.Data.Ident(Ident(Ident))
import Language.C.Data.Position(position)
import Language.C.Data(mkIdent,inputStreamFromString,newNameSupply,NodeInfo)
import Language.C.Syntax.AST(CCompoundBlockItem,CCompoundBlockItem(CBlockStmt),
                             CExpr,CStatement(CExpr),CExpression(CVar),
                             CDeclarationSpecifier(CTypeSpec),
                             CTypeSpecifier(CTypeDef))

import Data.Typeable(cast)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter,quoteExp,quotePat,quoteDec,quoteType),
                                 dataToExpQ)

import Quasi.EscapeTypeDefs

cExpr :: QuasiQuoter
cExpr = QuasiQuoter { quoteExp = quoteCExpr,
                      quotePat = undefined,
                      quoteDec = undefined,
                      quoteType = undefined }

quoteCExpr = quoteCCode expressionP

cStmt :: QuasiQuoter
cStmt = QuasiQuoter { quoteExp = quoteCStmt,
                      quotePat = undefined,
                      quoteDec = undefined,
                      quoteType = undefined }
quoteCStmt = quoteCCode statementP

cDecl :: QuasiQuoter
cDecl = QuasiQuoter { quoteExp = quoteCDecl,
                      quotePat = undefined,
                      quoteDec = undefined,
                      quoteType = undefined }
quoteCDecl = quoteCCode extDeclP

cTransL :: QuasiQuoter
cTransL = QuasiQuoter { quoteExp = quoteCTransL,
                        quotePat = undefined,
                        quoteDec = undefined,
                        quoteType = undefined }

quoteCTransL = quoteCCode translUnitP


quoteCCode parser s = do
  loc <- TH.location
  let (line,col) = TH.loc_start loc
      pos = position 0 (TH.loc_filename loc) line col
      antiquotes = map (mkIdent pos) $ scanForTokens s
      stream = inputStreamFromString s
      (typedefs,names) =
        (\(maybes,names) -> (map (\(Just a,b) -> a b) maybes,
                             map snd names)) $
        span (isJust . fst) $
        zip (map Just antiquotes ++ repeat Nothing) newNameSupply
  case execParser parser stream pos typedefs names of
    Right (val,_) -> do
      expr <- dataToExpQ lookForAntiquotes val
      return expr
    Left err -> fail (show err)

lookForAntiquotes :: Data b => b -> Maybe TH.ExpQ
lookForAntiquotes x = listToMaybe $ catMaybes [matchType x, matchStmt x, matchExpression x]

matchStmt :: Data b => b -> Maybe TH.ExpQ
matchStmt x =
  let y = cast x :: (Maybe (CCompoundBlockItem NodeInfo)) in
  case y of
    Just (CBlockStmt (CExpr (Just (CVar (Ident str hash _) _)) info)) ->
      case take 2 str of
        "$$" -> error "Type variable occured in statement context"
        ('$' : _) -> Just $ do
          let infoTH = dataToExpQ (const Nothing) info
          TH.appE (TH.varE (TH.mkName $ drop 1 str)) infoTH
        _ -> Nothing
    _ -> Nothing

matchExpression :: Data b => b -> Maybe TH.ExpQ
matchExpression x =
  let y = cast x :: (Maybe (CExpression NodeInfo)) in
  case y of
    Just (CVar (Ident str hash _) info) ->
      case take 2 str of
        "$$" -> error "Type variable occured in expression context"
        ('$' : _) -> Just $ do
          let infoTH = dataToExpQ (const Nothing) info
          TH.appE (TH.varE (TH.mkName $ drop 1 str)) infoTH
        _ -> Nothing
    _ -> Nothing

matchType :: Data b => b -> Maybe TH.ExpQ
matchType x =
  let y = cast x :: (Maybe (CDeclarationSpecifier NodeInfo)) in
  case y of
    Just (CTypeSpec (CTypeDef (Ident str hash _) info)) ->
      case take 2 str of
        "$$" -> Just $ do
          let infoTH = dataToExpQ (const Nothing) info
          TH.appE (TH.varE (TH.mkName $ drop 2 str)) infoTH
        _ -> Nothing
    _ -> Nothing
