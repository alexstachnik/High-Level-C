module Parser.Parser where

import AST.AST

import Data.Word
import Data.Maybe
import Data.Char

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

TokenParser{parens = cParens,
            identifier = cIdent,
            reservedOp = cReservedOp,
            reserved = cReserved,
            semi = cSemi,
            semiSep1 = cSemiSep1,
            whiteSpace = cWhiteSpace,
            charLiteral = cCharLit,
            stringLiteral = cStringLit,
            naturalOrFloat = cNaturalOrFloat,
            natural = cNatural,
            braces = cBraces} =
  makeTokenParser (javaStyle {reservedNames = ["if","while","return","function","passableStruct","notPassableStruct","Void","break","bind","continue","class","equal","family"],
                              reservedOpNames = ["=","+","-","*","/","&","@","<-"]})

stmtParser :: Parsec String u DoStmt
stmtParser =
  (do {lhs <- exprParser
      ;cReservedOp "="
      ;rhs <- exprParser
      ;return (Assignment lhs rhs)})
  <|>
  (do {cReserved "bind"
      ;varName <- cIdent
      ;fieldName <- tyParser
      ;return (MonadicBind (VarSymb varName) (HaskFunc makeStructFieldArg [ProxyExpr fieldName]))})



exprParser :: Parsec String u Expr
exprParser = buildExpressionParser exprTbl term <?> "expression"

exprTbl = [[Infix (cReservedOp "+" >> return (BinOpExpr HLCPlus)) AssocLeft]]

numToLit :: Either Integer Double -> HLCLit
numToLit (Left n) = IntLit n
numToLit (Right f) = DoubleLit f

toWord8 :: Int -> Word8
toWord8 n
  | (n >= 0) && (n <= 255) = fromIntegral n
  | otherwise = error "Out of bounds"

term = cParens exprParser
  <|> (cReserved "Void" >> return Void)
  <|> (cIdent >>= (return . LHSExpr . LHSVar . VarSymb))
  <|> (cNaturalOrFloat >>= (return . LitExpr . numToLit))
  <|> (cCharLit >>= return . LitExpr . CharLit)
  <|> (cStringLit >>= return . LitExpr . StrLit)

exportParser :: Parsec String u [Export]
exportParser = do
  ret <- cBraces $ cSemiSep1 $ optionMaybe exportLineParser
  return $ catMaybes ret

tyParser :: Parsec String u SomeType
tyParser = identTy <|> parenTy <|> familyTy
  where identTy = do
          name <- cIdent
          case isLower $ head name of
            True -> return $ IndefTy $ TyVar name
            False -> return $ DefTy $ TyCon name
        parenTy = cParens $ do
          (tyCon:args) <- many1 tyParser
          return $ TyConApp tyCon args
        familyTy = cParens $ do
          cReserved "family"
          family <- cIdent
          args <- many tyParser
          return $ TyFamily family args

exportLineParser :: Parsec String u Export
exportLineParser = do
  numArgs <- cNatural
  fname <- cIdent
  tyArgs <- many tyParser
  return $ Export (TyCon fname) tyArgs (fromIntegral numArgs)

funcArgParser :: Parsec String u (SomeType,VarSymb)
funcArgParser = do
  ty <- tyParser
  varname <- cIdent
  return (ty,VarSymb varname)

tyConstraintParser :: Parsec String u TyConstraint
tyConstraintParser = equalConstraint <|> classConstraint

equalConstraint :: Parsec String u TyConstraint
equalConstraint = do
  cReserved "equal"
  lhs <- tyParser
  rhs <- tyParser
  return $ TyEqual lhs rhs

classConstraint :: Parsec String u TyConstraint
classConstraint = do
  cReserved "class"
  className <- cIdent
  ty <- tyParser
  return $ TyConstraint (TyClass className) ty

funcParser :: Parsec String u HLCDecl
funcParser = do
  cReserved "function"
  retType <- tyParser
  fname <- cIdent
  tyArgs <- cParens (many cIdent)
  tyConstraints <- cParens (many tyConstraintParser)
  args <- cParens (many funcArgParser)
  body <- cBraces $ cSemiSep1 stmtParser
  return $ FuncDecl $ Function (TyCon fname) (map TyVar tyArgs) tyConstraints retType (map fst args) (map snd args) body

fieldDeclParser :: Parsec String u Field
fieldDeclParser = do
  ty <- tyParser
  fieldName <- cIdent
  return $ Field ty (TyCon fieldName)

passableStructParser :: Parsec String u HLCDecl
passableStructParser = do
  cReserved "passableStruct"
  name <- cIdent
  tyParams <- cParens (many cIdent)
  tyConstraints <- cParens (many tyConstraintParser)
  fields <- cBraces (cSemiSep1 fieldDeclParser)
  cons <- cBraces $ (cSemiSep1 stmtParser)
  dest <- cBraces $ (cSemiSep1 stmtParser)
  return $ StructDecl $ Struct (TyCon name) (map TyVar tyParams) tyConstraints fields True cons dest

notPassableStructParser :: Parsec String u HLCDecl
notPassableStructParser = do
  cReserved "notPassableStruct"
  name <- cIdent
  tyParams <- cParens (many cIdent)
  tyConstraints <- cParens (many tyConstraintParser)
  fields <- cBraces (cSemiSep1 fieldDeclParser)
  cons <- cBraces $ (cSemiSep1 stmtParser)
  dest <- cBraces $ (cSemiSep1 stmtParser)
  return $ StructDecl $ Struct (TyCon name) (map TyVar tyParams) tyConstraints fields True cons dest

tlDeclParser :: Parsec String u HLCDecl
tlDeclParser = funcParser <|> passableStructParser <|> notPassableStructParser

tluParser :: Parsec String u TLU
tluParser = do
  cWhiteSpace
  exports <- exportParser
  tlDecls <- many tlDeclParser
  eof
  return $ TLU exports tlDecls

parseFile :: String -> TLU
parseFile str = case parse tluParser "" str of
  Left err -> error $ show err
  Right tlu -> tlu
