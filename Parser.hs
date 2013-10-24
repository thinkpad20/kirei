import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Control.Applicative hiding ((<|>), many)

type Name = String

data Expr = Term Term
          | Apply Expr Expr
          | If Expr Expr Expr
          | Lambda [Name] Expr
          deriving (Show)

data Term = Bool Bool
          | Num Double
          | String String
          | Identifier Name
          | Symbol Name
          | Parens Expr
          deriving (Show)

keywords = ["if", "then", "else", "True", "False", "->", 
            "=>", "fn", "let", "def", "sig", ":", "|"]
lexeme p = p <* spaces
schar = lexeme . char

keyword k = lexeme . try $
  string k <* notFollowedBy alphaNum

checkKeyword s = if s `elem` keywords
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

pIdentifier :: Parser String
pIdentifier = lexeme . try $ do
  ident <- many1 letter
  checkKeyword ident

pSymbol :: Parser String
pSymbol = lexeme . try $ do
  sym <- lexeme $ many1 $ oneOf "><=+-*/^~!%@&$"
  checkKeyword sym

pParens :: Parser Expr
pParens = between (schar '(') (schar ')') pExpr

pTerm :: Parser Term
pTerm = choice [ Bool       <$> pBool,
                 Num        <$> pDouble,
                 String     <$> pString,
                 Identifier <$> pIdentifier,
                 Symbol     <$> pSymbol,
                 Parens     <$> pParens]

pApply :: Parser Expr
pApply = chainl1 pTerm' (pure Apply)

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

pLambda :: Parser Expr
pLambda = Lambda <$ keyword "fn" <*> many pIdentifier
                 <* keyword "=>" <*> pExpr


pExpr :: Parser Expr
pExpr = pIf <|> pLambda <|> pApply

test parser = parse (spaces *> parser <* eof) ""