module Parser {-(grab, test, Expr(..))-} where

import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding ((<|>), many, optional)

type Name = String
type Matches = [(Expr, Expr)]

data TypeName = TypeName Name [TypeName]

instance Show TypeName where
  show (TypeName n []) = n
  show (TypeName n params) = intercalate " " (n: (show <$> params))

data Constructor = Constructor Name [TypeName] deriving Show

data Expr =
  Bool Bool
  | Number Double
  | String String
  | Symbol Name
  | Var Name
  | Underscore
  | If Expr Expr Expr
  | Let Name Expr (Maybe Expr)
  | Apply Expr Expr
  | Dotted Expr Expr
  | Comma Expr Expr
  | Case Expr Matches
  | Tuple [Expr]
  | Lambda Name Expr
  | List ListLiteral
  | Datatype Name [Constructor] (Maybe Expr)
  deriving (Show)

data ListLiteral =
  ListLiteral [Expr]
  | ListRange Expr Expr
  deriving (Show)

skip :: Parser ()
skip = spaces *> (lineComment <|> spaces) where
  lineComment = do
    char '#'
    many $ noneOf "\n"
    newline <|> (eof >> return ' ')
    return ()

keywords = ["if", "then", "else", "True", "False",
            "let", "def", "sig", "case", "of"]
keySyms = ["->", "=>", "|", "=", ";", "\\", "/*"]
lexeme p = p <* skip
schar = lexeme . char

keyword k = lexeme . try $
  string k <* notFollowedBy alphaNum

keysim k = lexeme . try $
  string k <* notFollowedBy (oneOf "><=+-*/^~!%@&:.")

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
    keysim "."
    ds' <- many1 digit
    return $ read (ds ++ "." ++ ds')

pString :: Parser String
pString = lexeme . between (char '"') (char '"') . many1 $ noneOf "\""

pVariable :: Parser String
pVariable = checkParse $ do
  first <- letter <|> char '$' <|> char '_'
  rest <- many (letter <|> digit <|> char '$' <|> char '_')
  return (first : rest)

pSymbol :: Parser String
pSymbol = checkParse $ many1 $ oneOf "><=+-*/^~!%@&$:"

pList :: Parser Expr
pList = List <$> between (schar '[') (schar ']') get where
  get = try (do
    start <- pExpr
    keysim ".."
    stop <- pExpr
    return $ ListRange start stop)
    <|> ListLiteral <$> (sepBy pExpr (schar ','))

pParens :: Parser Expr
pParens = do
  es <- between (schar '(') (schar ')') $ sepBy1 pExpr (schar ',')
  case es of
    [e] -> return e
    es -> return $ Tuple es

pDatatype :: Parser Expr
pDatatype = Datatype <$ keyword "datatype" <*> pVariable
                     <* keysim "=" <*> pConstructors
                     <* keysim ";" <*> optionMaybe pExprs where
  pSingleTypeName = TypeName <$> pVariable <*> pure []
  pComplexTypeName = TypeName <$ schar '(' <*> pVariable
                              <*> many pSingleTypeName <* schar ')'
  pConstructor = do
    name <- pVariable
    argumentTypes <- many (pSingleTypeName <|> pComplexTypeName)
    return $ Constructor name argumentTypes
  pConstructors = sepBy1 pConstructor (schar '|')

pCase :: Parser Expr
pCase = Case <$ keyword "case" <*> pExpr
             <* keyword "of"   <*> sepBy1 match (schar '|') where
  match = do
    pattern <- pExpr
    keysim "->"
    result <- pExprs
    return (pattern, result)

pTerm :: Parser Expr
pTerm = choice [ Bool   <$> pBool,
                 Number <$> pDouble,
                 String <$> pString,
                 Var    <$> pVariable,
                 Symbol <$> pSymbol,
                 pParens,
                 pLambda,
                 pCase,
                 pList]

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
    keysim "."
    y <- pExpr
    parseRest (Dotted res y)
    <|> return res

pIf :: Parser Expr
pIf = If <$ keyword "if"   <*> pExpr
         <* keyword "then" <*> pExpr
         <* keyword "else" <*> pExpr

pLambda :: Parser Expr
pLambda = do
  keysim "\\"
  vars <- many pVariable
  keysim "->"
  expr <- pExpr
  return $ lambda vars expr where
    lambda [] e = e
    lambda (v:vs) e = Lambda v (lambda vs e)

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
    f (a:as) e = Lambda a (f as e)

pExpr :: Parser Expr
pExpr = choice [pIf, pLet, pDatatype, pApply]

pExprs :: Parser Expr
pExprs = chainl1 pExpr (schar ',' *> pure Comma)

grab :: String -> Expr
grab s = case parse (skip *> pExprs
                     <* many (keysim ";")
                     <* eof) "" s of
  Right val -> val
  Left err -> error $ show err

test parser = parse (skip *> parser <* eof) ""
