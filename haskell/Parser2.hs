import Text.ParserCombinators.Parsec
import Control.Monad (liftM)

data Expr = Term Term
          | Apply Expr Expr
          | If Expr Expr Expr
          deriving (Show)

data Term = Bool Bool
          | Num Double
          | String String
          | Identifier String
          | Parens Expr
          deriving (Show)

sstring s = spaces >> string s
schar c = spaces >> char c

keyword k = do
  kw <- try (sstring k)
  notFollowedBy alphaNum
  return kw

pBool :: Parser Bool
pBool = do
  bool <- keyword "True" <|> keyword "False"
  case bool of
    "True" -> return True
    "False" -> return False

pDouble :: Parser Double
pDouble = do
  ds <- many1 digit
  dot <- optionMaybe $ char '.'
  case dot of
    Nothing -> return $ read ds
    _ -> do
      ds' <- many1 digit
      return $ read (ds ++ "." ++ ds')

pString :: Parser String
pString = do
  char '"'
  str <- many1 $ noneOf "\""
  char '"'
  return str

pIdentifier :: Parser String
pIdentifier = spaces >> many1 letter

pParens :: Parser Expr
pParens = do
  schar '('
  expr <- pExpr
  schar ')'
  return expr

pTerm :: Parser Term
pTerm = try (liftM Bool pBool)
  <|> try (liftM Num pDouble)
  <|> try (liftM String pString)
  <|> try (liftM Identifier pIdentifier)
  <|> try (liftM Parens pParens)

-- TODO: make this left-associative
pApply :: Parser Expr
pApply = do
  term <- try pTerm'
  mApp <- spaces >> optionMaybe pApply
  return $ case mApp of
    Just app -> Apply term app
    Nothing -> term

-- pulls "parens" expressions out of terms
pTerm' :: Parser Expr
pTerm' = do
  term <- try pTerm
  case term of 
    Parens expr -> return expr
    otherwise -> return $ Term term

pIf :: Parser Expr
pIf = do
  keyword "if"
  cond <- pExpr
  keyword "then"
  ifTrue <- pExpr
  keyword "else"
  ifFalse <- pExpr
  return $ If cond ifTrue ifFalse

pExpr :: Parser Expr
pExpr = spaces >> (try pIf <|> pTerm')

test parser = parse parser ""