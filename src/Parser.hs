module Parser (grab, Expr(..)) where

import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding ((<|>), many, optional)

type Name = String

data Expr =
  Bool Bool
  | Number Double
  | String String
  | Symbol Name
  | Var Name
  | If Expr Expr Expr
  | Let Name Expr (Maybe Expr)
  | Apply Expr Expr
  | Dotted Expr Expr
  | Comma Expr Expr
  | Tuple [Expr]
  | Lambda [Name] Expr
  deriving (Show)

skip :: Parser ()
skip = spaces *> (lineComment <|> spaces) where
  lineComment = do
    char '#'
    many $ noneOf "\n"
    char '\n'
    return ()

keywords = ["if", "then", "else", "True", "False", "let", "def", "sig"]
keySyms = ["->", "=>", ":", "|", "=", ";", "\\", "/*"]
lexeme p = p <* skip
schar = lexeme . char

keyword k = lexeme . try $
  string k <* notFollowedBy alphaNum

keysim k = lexeme . try $
  string k <* notFollowedBy (oneOf "><=+-*/^~!%@&")

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
pVariable = checkParse $ many1 (letter <|> char '$')

pSymbol :: Parser String
pSymbol = checkParse $ many1 $ oneOf "><=+-*/^~!%@&$"

pParens :: Parser Expr
pParens = do
  es <- between (schar '(') (schar ')') $ sepBy1 pExpr (schar ',')
  case es of
    [e] -> return e
    es -> return $ Tuple es

pTerm :: Parser Expr
pTerm = choice [ Bool   <$> pBool,
                 Number <$> pDouble,
                 String <$> pString,
                 Var    <$> pVariable,
                 Symbol <$> pSymbol,
                 pParens,
                 pLambda]

pApply :: Parser Expr
pApply = pDotted >>= \res -> parseRest res where
  parseRest res = do
    y <- pTerm -- run the parser again
    case y of
      (Symbol s) -> parseRest (Apply y res) -- if a symbol, flip the order
      _ -> parseRest (Apply res y) -- otherwise, keep chainin' along
    <|> return res -- at some point the second parse will fail; then
                   -- return what we have so far

pDotted :: Parser Expr
pDotted = pTerm >>= \res -> parseRest res where
  parseRest res = do
    schar '.'
    y <- pExpr
    parseRest (Dotted res y)
    <|> return res

pIf :: Parser Expr
pIf = If <$ keyword "if"   <*> pExpr
         <* keyword "then" <*> pExpr
         <* keyword "else" <*> pExpr

pLambda :: Parser Expr
pLambda = Lambda <$ keysim "\\" <*> many pVariable
                 <* keysim "=>" <*> pExpr

pLet :: Parser Expr
pLet = do
  keyword "let"
  fname <- pVariable
  args <- many pVariable
  keysim "="
  expr <- pExprs
  keysim ";"
  next <- optionMaybe pExprs
  return $ Let fname (f args expr) next where
    f [] e = e
    f (a:as) e = Lambda [a] (f as e)

pExpr :: Parser Expr
pExpr = choice [pIf, pLet, pApply]

pExprs :: Parser Expr
pExprs = chainl1 pExpr (schar ',' *> pure Comma)

grab s = case parse (spaces *> pExprs
                     <* many (keysim ";")
                     <* eof) "" s of
  Right val -> val
  Left err -> error $ show err

test parser = parse (spaces *> parser <* eof) ""
