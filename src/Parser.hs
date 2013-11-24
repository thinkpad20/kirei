module Parser {-(grab, test, Expr(..))-} where

import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding ((<|>), many, optional)
import Data.Monoid
import Debug.Trace

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
  | ExtendedLambda Matches (Maybe Expr)
  | List ListLiteral
  | Datatype Name [Name] [Constructor] (Maybe Expr)
  deriving (Show)

data ListLiteral =
  ListLiteral [Expr]
  | ListRange Expr Expr
  deriving (Show)

data InString =
  Plain String
  | InterShow InString Expr InString
  | Interpolate InString Expr InString

instance Show InString where
  show (Plain s) = show s
  show (InterShow s e s') = show s ++ " ++ show (" ++ show e ++ ") ++ " ++ show s'
  show (Interpolate s e s') = show s ++ " ++ (" ++ show e ++ ") ++ " ++ show s'

-- | Compiles an interpolated string into an expression via concatenations
isToExpr :: InString -> Expr
isToExpr (Plain s) = String s
isToExpr (InterShow s1 e s2) = case (s1, s2) of
  (Plain "", Plain "") -> sh e
  (is, Plain "") -> Apply (pp $ isToExpr is) (sh e)
  (Plain "", is) -> Apply (pp $ sh e) (isToExpr is)
  (is1, is2) -> Apply (pp (isToExpr is1)) (Apply (pp $ sh e) (isToExpr is2))
  where sh = Apply (Var "show")
        pp = Apply (Symbol "++")
isToExpr (Interpolate s1 e s2) = case (s1, s2) of
  (Plain "", Plain "") -> sh e
  (is, Plain "") -> Apply (pp $ isToExpr is) e
  (Plain "", is) -> Apply (pp e) (isToExpr is)
  (is1, is2) -> Apply (pp (isToExpr is1)) (Apply (pp e) (isToExpr is2))
  where sh = Apply (Var "show")
        pp = Apply (Symbol "++")

pInString :: Parser InString
pInString = do
  first@(Plain s) <- Plain <$> (many $ noneOf "#")
  case s of
    "" -> return first
    _ -> choice [
            try $ pShowExpr $ InterShow first,
            try $ pLiteralExpr $ Interpolate first,
            join first <$> (pure prepend <*> char '#' <*> pInString),
            return first
          ]
  where
    pShowExpr f = f <$> (sstring "#{" *> pExpr) <*> (char '}' *> pInString)
    pLiteralExpr f = f <$> (sstring "#[" *> pExpr) <*> (char ']' *> pInString)
    -- prepends a character onto an InString
    prepend c (Plain s) = Plain (c : s)
    prepend c (Interpolate s e s') = Interpolate (prepend c s) e s'
    prepend c (InterShow s e s') = InterShow (prepend c s) e s'
    -- joins a Plain instring onto another instring
    join (Plain s) (Plain s') = Plain (s ++ s')
    join is1 (InterShow s' e s'') = InterShow (join is1 s') e s''
    join is1 (Interpolate s' e s'') = Interpolate (join is1 s') e s''

pPlain :: Parser InString
pPlain = Plain <$> (anyChar `manyTill` try end) where
  end = string "#{" <|> string "#[" <|> (eof >> return "")

skip :: Parser ()
skip = spaces *> (blockCom <|> lineComment <|> spaces) where
  lineComment = do
    char '#'
    many $ noneOf "\n"
    newline <|> (eof >> return ' ')
    return ()
  blockCom = string "/*" >> manyTill anyChar (try $ string "*/") >> return ()


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

symChars = "><=+-*/^~!%@&$:λ."

symbolChars :: Parser Char
symbolChars = oneOf symChars

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

-- | Extended let - handles a more general case which gets compiled down into
-- a regular set statement down the line
pExtendedLet :: Parser Expr
pExtendedLet = do
  keyword "let"
  matches <- sepBy1 getPattern (keysim "|")
  keysim ";"
  next <- optionMaybe pExprs
  case matches of
    -- with a single solitary pattern, we don't need a case
    -- statement, just its arguments, name and body
    [p] -> do
      let (name, args, body) = (getName p, getArgs $ fst p, getBody p)
      return $ Let name (f args body) next
    ps -> do
      let matches = zip (getLeft <$> ps) (getRight <$> ps)
          name :: Name
          name = getName $ head ps
          arg = "__arg"
          allSame = and $ map (== name) (getName <$> tail ps)
      if allSame then
        return $ Let name (Lambda arg $ Case (Var arg) matches) next
        else error $ "Some patterns don't conform"
  where
    getPattern = pure (,) <*> pExpr <* keysim "=" <*> pExprs
    -- gets the name of the function out of a pattern
    getName (Var n, _) = n
    getName (Symbol s, _) = s
    getName (Apply a b, e) = getName (a, e)
    getName p = error $ "Invalid pattern: " ++ show p
    getArgs (Var n) = []
    getArgs (Symbol s) = []
    getArgs (Apply a b) = getArgs a ++ getArgs b
    --getArgs (Tuple es) = getArgs <$> es
    getBody = undefined
    getLeft = undefined
    getRight = undefined
    f [] e = e
    f (a:as) e = Lambda a (f as e)

-- Kinda hacky, needs to be fixed
pLet :: Parser Expr
pLet = do
  keyword "let"
  pattern <- pExpr
  keysim "="
  body <- pExprs
  keysim ";"
  next <- optionMaybe pExprs
  return $ Let (head $ args pattern) (f (tail $ args pattern) body) next where
    args p = case p of
      Var v -> [v]
      Symbol s -> [s]
      Apply a b -> args a ++ args b
      _ -> error $ "Illegal pattern " ++ show p
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
