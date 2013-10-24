import Text.ParserCombinators.Parsec hiding (optional)
import Text.Parsec.Expr
import Control.Applicative hiding ((<|>), many)

type Name = String

data Expr = Term Term
          | Apply Expr Expr
          | If Expr Expr Expr
          | Lambda [Name] Expr
          | Let String Expr (Maybe Expr)
          deriving (Show)

data Term = Bool Bool
          | Num Double
          | String String
          | Identifier Name
          | Symbol Name
          | Parens Expr
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

pIdentifier :: Parser String
pIdentifier = checkParse $ many1 letter

pSymbol :: Parser String
pSymbol = checkParse $ many1 $ oneOf "><=+-*/^~!%@&$"

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
pLambda = Lambda <$ keysim "\\" <*> many pIdentifier
                 <* keysim "=>" <*> pExpr

pLet :: Parser Expr
pLet = Let <$ keyword "let" <*> pIdentifier
           <* keysim "=" <*> pExpr 
           <* keysim ";" <*> optional pExpr

pExpr :: Parser Expr
pExpr = choice [pIf, pLambda, pLet, pApply]

test parser = parse (spaces *> parser <* eof) ""