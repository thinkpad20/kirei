module Parser3 where

import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding ((<|>), many, optional)

data Statement =
  Expr Expr
  | Assign Let
  deriving (Show)

data Sig =
  Type String
  | From Sig Sig
  deriving (Show)

-- later add a maybe sig
type FreeVar = String

data Let = Let FreeVar Expr deriving (Show)

data Expr =
  Term Term
  | Apply Expr Expr
  | If Expr Expr Expr
  | Comma Expr Expr
  deriving (Show)

data Term =
  Number Double
  | Bool Bool
  | Variable String
  | Symbol String
  | String String
  | Parens Expr
  | Lambda [FreeVar] [Let] Expr
  deriving (Show)


keywords = ["if", "then", "else", "True", "False", "let", "def", "sig"]
keySyms = ["->", "=>", ":", "|", "=", ";", "\\"]
lexeme p = p <* spaces
schar = lexeme . char

keyword k = lexeme . try $
  string k <* notFollowedBy alphaNum

keysim k = lexeme . try $
  string k <* notFollowedBy (oneOf "><=+-*/^~!%@&$")

checkParse p = lexeme . try $ do
  s <- p
  if s `elem` (keywords ++ keySyms)
    then unexpected $ "reserved word " ++ show s
    else return s

pBool :: Parser Bool
pBool = (True <$ keyword "True") <|> (False <$ keyword "False")

pDouble :: Parser Double
pDouble = lexeme $ do
  ds <- many1 digit
  option (read ds) $ do
    char '.'
    ds' <- many1 digit
    return $ read (ds ++ "." ++ ds')

pString :: Parser String
pString = lexeme . between (char '"') (char '"') . many1 $ noneOf "\""

pVariable :: Parser String
pVariable = checkParse $ many1 letter

pSymbol :: Parser String
pSymbol = checkParse $ many1 $ oneOf "><=+-*/^~!%@&$"

pParens :: Parser Expr
pParens = between (schar '(') (schar ')') pExpr

pTerm :: Parser Term
pTerm = choice [ Number   <$> pDouble,
                 String   <$> pString,
                 Variable <$> pVariable,
                 Symbol   <$> pSymbol,
                 Parens   <$> pParens,
                 pLambda ]

pApply :: Parser Expr
pApply = chainl1 pTerm' $ pure Apply

-- pulls "parens" expressions out of terms
pTerm' :: Parser Expr
pTerm' = do
  term <- pTerm
  case term of
    Parens expr -> return expr
    _ -> return $ Term term

pIf :: Parser Expr
pIf = If <$ keyword "if"   <*> pExpr
         <* keyword "then" <*> pExpr
         <* keyword "else" <*> pExpr

pLambda :: Parser Term
pLambda = Lambda <$ keysim "\\" <*> many pVariable
                 <* keysim "=>" <*> many pLet
                                <*> pExpr

pLet :: Parser Let
pLet = Let <$ keyword "let" <*> pVariable
           <* keysim "=" <*> pExpr
           <* keysim ";"

pExpr :: Parser Expr
pExpr = choice [pIf, pApply]

pStatement :: Parser Statement
pStatement = choice [Assign <$> pLet,
                     Expr <$> (pExpr <* keysim ";")]

pStatements :: Parser [Statement]
pStatements = many pStatement

test parser = parse (spaces *> parser <* eof) ""
