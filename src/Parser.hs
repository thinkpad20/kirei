module Parser {-(grab, test, Expr(..))-} where

import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding ((<|>), many, optional)

type Name = String
type Matches = [(Expr, Expr)]

data TypeName = TypeName Name [TypeName] deriving (Ord, Eq)

(~>) = flip (.)
infixr 9 ~>

instance Show TypeName where
  show (TypeName n []) = n
  show (TypeName n params) = intercalate " " (n: (show <$> params))

data Constructor = Constructor Name [TypeName] deriving (Show, Ord, Eq)

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
  | Datatype Name [Name] [Constructor] (Maybe Expr)
  deriving (Eq, Ord)

data ListLiteral =
  ListLiteral [Expr]
  | ListRange Expr Expr
  deriving (Show, Eq, Ord)

instance Show Expr where
  show e = case e of
    Bool b -> show b
    Number n -> show n
    String s -> show s
    Symbol op -> op
    Var v -> v
    Underscore -> "_"
    If c t f -> "if " ++ show c ++ " then " ++ show t ++ " else " ++ show f
    Let n e1 e2 -> "let " ++ n ++ " = " ++ show e1 ++ (case e2 of
      Nothing -> "; "
      Just e2 -> show e2 ++ "; ")
    Apply a b -> show a ++ " " ++ show b
    Dotted a b -> show a ++ "." ++ show b
    Comma a b -> show a ++ ", " ++ show b
    Case e matches -> "case " ++ show e ++ " of " ++ sh matches where
      sh = map s ~> intercalate "|"
      s (ex, exs) = show ex ++ " -> " ++ show exs
    Tuple es -> "(" ++ intercalate ", " (show <$> es) ++ ")"
    Lambda n e -> "\\" ++ n ++ " -> " ++ show e
    List (ListLiteral es) -> "[" ++ intercalate ", " (show <$> es) ++ "]"
    List (ListRange start stop) -> "[" ++ show start ++ ".." ++ show stop ++ "]"

skip :: Parser ()
skip = spaces *> (lineComment <|> spaces) where
  lineComment = do
    char '#'
    many $ noneOf "\n"
    newline <|> (eof >> return ' ')
    return ()

precedences = [
                ["!", "$"], -- lowest precedence
                [">>", ">>="],
                ["||", "&&"],
                ["==", "!=", "<", ">", "<=", ">="],
                ["::", "++"],
                ["+",  "-"],
                ["*",  "/"],
                ["^"],
                ["~>", "<~", "!!"] --highest precedence
              ]

pBinary :: [[String]] -> Parser Expr
pBinary = pFrom where
  pFrom [] = pApply
  pFrom (symbols:sss) = pRightAssoc (pFrom sss) (choice $ getSym <$> symbols)

keywords = ["if", "then", "else", "True", "False",
            "let", "sig", "case", "of"]
keySyms = ["->", "|", "=", ";", "\\"]
lexeme p = p <* skip
sstring = lexeme . string
schar = lexeme . char

symbolChars :: Parser Char
symbolChars = oneOf "><=+-*/^~!%@&$:λ."

getSym :: String -> Parser String
getSym s = try $ do
  sym <- pSymbol
  if sym == s
    then return sym
    else unexpected $ concat ["Expected a '", s, "' but got a '", sym, "'"]

keyword k = lexeme . try $
  string k <* notFollowedBy alphaNum

keysim k = lexeme . try $
  string k <* notFollowedBy symbolChars

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
pVariable = checkParse $ (:) <$> first <*> rest where
  first = letter <|> char '$' <|> char '_'
  rest = many (letter <|> digit <|> char '$' <|> char '_')

pSymbol :: Parser String
pSymbol = checkParse $ many1 symbolChars
  <|> between (char '`') (char '`') pVariable

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
  es <- between (schar '(') (schar ')') $ sepBy pExpr (schar ',')
  case es of
    [e] -> return e
    es -> return $ Tuple es

pDatatype :: Parser Expr
pDatatype = Datatype <$ keyword "datatype" <*> pVariable <*> many pVariable
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
             <* keyword "of"   <*> sepBy1 pMatch (schar '|') where
  pMatch = (,) <$> pExpr <* keysim "->" <*> pExprs

pVariableOrUnderscore :: Parser Expr
pVariableOrUnderscore = do
  v <- pVariable
  if v == "_" then return Underscore else return $ Var v

pTerm :: Parser Expr
pTerm = choice [ Bool   <$> pBool,
                 Number <$> pDouble,
                 String <$> pString,
                 pVariableOrUnderscore,
                 pParens,
                 pLambda,
                 pCase,
                 pList]

pRightAssoc :: Parser Expr -> Parser String -> Parser Expr
pRightAssoc pLeft pSym = optionMaybe pLeft >>= loop where
  loop :: Maybe Expr -> Parser Expr
  loop left = do
    sym <- pSym
    right <- optionMaybe $ pRightAssoc pLeft pSym
    return $ case (left, right) of
      (Nothing, Nothing) -> (Symbol sym)
      (Just left, Nothing) -> Apply (Symbol sym) left
      (Nothing, Just right) ->
        Lambda "x" (Apply (Apply (Symbol sym) (Var "x")) right)
      (Just left, Just right) ->
        Apply (Apply (Symbol sym) left) right
    <|> case left of
      Nothing   -> unexpected "Empty expression"
      Just left -> return left

pApply :: Parser Expr
pApply = pDotted >>= parseRest where
  parseRest res = do -- res is a
    term <- pDotted -- run the parser again
    parseRest (Apply res term)
    <|> return res -- at some point the second parse will fail; then
                   -- return what we have so far

pDotted :: Parser Expr
pDotted = pTerm >>= parseRest where
  parseRest res = do
    keysim "."
    y <- pTerm
    parseRest (Dotted res y)
    <|> return res

pIf :: Parser Expr
pIf = If <$ keyword "if"   <*> pExpr
         <* keyword "then" <*> pExprs
         <* keyword "else" <*> pExprs

pLambda :: Parser Expr
pLambda = do
  keysim "\\" <|> keysim "λ"
  vars <- many pVariable
  keysim "->" <|> keysim "."
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
pExpr = choice [pIf, pLet, pDatatype, pBinary precedences]

pExprs :: Parser Expr
pExprs = chainl1 pExpr (schar ',' *> pure Comma)

grab :: String -> Expr
grab s = case parse (skip *> pExprs
                     <* many (keysim ";")
                     <* eof) "" s of
  Right val -> val
  Left err -> error $ show err

test parser = parse (skip *> parser <* eof) ""
