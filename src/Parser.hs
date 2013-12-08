{-# LANGUAGE LambdaCase #-}
module Parser (grab) where

import Text.Parsec hiding (parse)
import Data.List
import Control.Applicative hiding ((<|>), many, optional)
import Data.Monoid
import Debug.Trace
import Common
import AST
import Types
import Data.Char (isLower)
import qualified Data.Map as M

type UserState = PrecedenceTable
type PrecedenceTable = M.Map Int [Precedence]
type Source = String
type Parser = ParsecT Source UserState IO

data Precedence = LeftAssoc Name | RightAssoc Name | NonAssoc Name



skip :: Parser ()
skip = spaces *> (blockCom <|> lineComment <|> spaces) where
  lineComment = do
    char '#'
    many $ noneOf "\n"
    newline <|> (eof >> return ' ')
    return ()
  blockCom = string "/*" >> manyTill anyChar (try $ string "*/") >> return ()


precedences :: PrecedenceTable
precedences = M.fromList [
                (0, [l "!", r "$"]), -- lowest precedence
                (1, [l ">>", l ">>="]),
                (2, [r "||"]),
                (3, [r "&&"]),
                (4, [n "==", n "!=", n "<", n ">", n "<=", n ">="]),
                (5, [r "::", r "++"]),
                (6, [l "+", l "-"]),
                (7, [l "*", l "/"]),
                (8, [r "^"]),
                (9, [l "~>", r "<~", r "!!"]) --highest precedence
              ]
              where l = LeftAssoc; r = RightAssoc; n = NonAssoc

pBinary :: Parser Expr
pBinary = getState >>= pFrom 0 where
  pFrom :: Int -> PrecedenceTable -> Parser Expr
  pFrom n precTable | n > 9     = pApply
                    | otherwise = runPrecs precs where
    precs :: [Precedence]
    precs = M.findWithDefault [] n precTable
    runPrecs :: [Precedence] -> Parser Expr
    runPrecs [] = pFrom (n+1) precTable
    runPrecs (RightAssoc sym:syms) = pRightAssoc (runPrecs syms) (getSym sym)
    runPrecs (LeftAssoc sym:syms)  = pRightAssoc (runPrecs syms) (getSym sym)
    runPrecs (NonAssoc sym:syms)   = pRightAssoc (runPrecs syms) (getSym sym)

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
  first@(Plain s) <- Plain <$> (many $ noneOf "#\"")
  choice [
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

pString :: Parser Expr
pString = isToExpr <$> lexeme (between (char '"') (char '"') pInString)

pVariable :: Parser String
pVariable = pTypeName <|> (checkParse $ (:) <$> first <*> rest) where
  first = lower <|> char '$' <|> char '_'
  rest = many (alphaNum <|> char '$' <|> char '_')

pTypeVariable :: Parser String
pTypeVariable = checkParse $ (:) <$> lower <*> many alphaNum

pTypeName :: Parser String
pTypeName = lexeme $ (:) <$> upper <*> many alphaNum

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

pADT :: Parser Expr
pADT = ADT <$ keyword "adt" <*> pTypeName <*> many pTypeVariable
           <* keysim "=" <*> pConstructors
           <* keysim ";" <*> optionMaybe pExprs where
  pConstructor = try pSymConstructor <|> pVarConstructor
  pSymConstructor = do
    leftT <- pType
    name <- pSymbol
    rightT <- pType
    return $ Constructor name [leftT, rightT]
  pVarConstructor = pure Constructor <*> pTypeName <*> many pTTerm
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
                 pString,
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
        Lambda (Var "_a") (Apply (Apply (Symbol sym) (Var "_a")) right)
      (Just left, Just right) ->
        Apply (Apply (Symbol sym) left) right
    <|> case left of
      Nothing   -> unexpected "Empty expression"
      Just left -> return left

pApply :: Parser Expr
pApply = pDotted >>= parseRest where
  parseRest res = do -- res is a parsed expression
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
  vars <- many1 pVariable
  keysim "->" <|> keysim "."
  expr <- pExpr
  return $ lambda vars expr where
    lambda [] e = e
    lambda (v:vs) e = Lambda (Var v) (lambda vs e)

pLet :: Parser Expr
pLet = do
  name  <- keyword "let" *> (pVariable <|> pSymbol)
  mType <- optionMaybe (keysim ":" *> pType)
  body  <- keysim "=" *> pExprs <* keysim ";"
  next  <- optionMaybe pExprs
  let thisLet = Let name body next
  return $ case mType of
    Nothing -> thisLet
    Just typ -> Sig name typ $ Just thisLet

pSig :: Parser Expr
pSig = Sig <$ keyword "sig" <*> (pVariable <|> pSymbol)
           <* keysim ":" <*> pType <* keysim ";" <*> optionMaybe pExprs

pTTuple :: Parser Type
pTTuple = pTApply `sepBy` (keysim ",") >>= \case
  [t] -> return t
  ts  -> return $ TTuple ts

pType :: Parser Type
pType = chainr1 pTTuple (keysim "->" *> pure (:=>))

pTApply :: Parser Type
pTApply = chainl1 pTTerm (pure TApply)

pTTerm = choice [pTParens, pTVar, pTConst, pListType] where
  pTParens = between (schar '(') (schar ')') pType
  pTVar = TVar <$> pTypeVariable
  pTConst = TConst <$> pTypeName
  pListType = do
    keysim "["
    term <- pTTerm
    keysim "]"
    return $ TApply (TConst "[]") term

pFixity :: Parser Expr
pFixity = choice [pInfixL, pInfixR, pInfix] *> pExpr where
  pInfixR = go "infixr" RightAssoc
  pInfixL = go "infixl" LeftAssoc
  pInfix  = go "infix" NonAssoc
  go key assoc = do
    sstring key
    level <- read <$> many1 digit <* spaces
    symbol <- pSymbol
    keysim ";"
    addFixity level assoc symbol

addFixity :: Int -> (String -> Precedence) -> String -> Parser ()
addFixity level assoc symbol = modifyState $ add where
  add table =
    let precs = M.findWithDefault [] level table in
    M.insert level (assoc symbol : precs) table

pExpr :: Parser Expr
pExpr = choice [pFixity, pIf, pSig, pLet, pADT, pBinary]

pExprs :: Parser Expr
pExprs = chainl1 pExpr (schar ',' *> pure Comma)

parse :: Parser a -> UserState -> Source -> IO (Either ParseError a)
parse p u s = runParserT p u "" s

grab :: String -> IO Expr
grab s = parse parseIt precedences s >>= \case
  Right expr -> return expr
  Left err -> error $ show err
  where parseIt = skip *> pExprs <* many (keysim ";") <* eof

-- test parser = parse (skip *> parser <* eof) ""

