module Parse where

import Text.ParserCombinators.Parsec
import AST
import Data.Monoid
import Data.List

sstring s = spaces >> string s
schar c = spaces >> char c

getIdent :: Parser Char -> Parser String
getIdent p = spaces >> do
  let validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  first <- p
  rest <- optionMaybe $ many $ oneOf validChars
  case rest of
    Nothing -> return [first]
    Just cs -> return (first:cs)

parseUpperName, parseLowerName :: Parser String
parseUpperName = getIdent upper
parseLowerName = getIdent lower

parseStringLiteral :: Parser String
parseStringLiteral = spaces >> do
  char '"'
  content <- many $ noneOf "\""
  char '"'
  return content

parseDouble :: Parser Double
parseDouble = spaces >> do
  ds <- many1 digit
  dot <- optionMaybe $ char '.'
  case dot of
    Nothing -> return $ read ds
    _ -> many1 digit >>= \ds' -> return $ read (ds ++ "." ++ ds')

parseNum, parseVarName, parseString :: Parser Term
parseNum = fmap Num parseDouble
parseVarName = fmap VarName parseLowerName
parseString = fmap String parseStringLiteral

parseDec :: Parser Dec
parseDec = do
  vName <- parseLowerName
  schar '='
  expr <- parseExpr
  return $ Dec vName patterns expr

parseLet :: Parser Let
parseLet = do
  sstring "let"
  decs <- sepBy1 parseDec (schar '|')
  return $ Let decs

parseTerm :: Parser Term
parseTerm = parseNum <|> parseVarName <|> parseString

parseIf :: Parser Expr
parseIf = do
  sstring "if"
  cond <- parseExpr
  sstring "then"
  ifTrue <- parseExpr
  sstring "else"
  ifFalse <- parseExpr
  return $ If cond ifTrue ifFalse

parseExpr :: Parser Expr
parseExpr = try parseIf <|> fmap Term parseTerm

parseLetStmts :: Parser Statement
parseLetStmts = do
  ss <- sepBy1 parseLet (schar ',')
  return $ LetStmts ss

-- putting this one on hold for now...
parseImportStmt :: Parser Statement
parseImportStmt = fmap ImportStmt (parseImport <|> parseFromImport) where
  parseImport = do
    sstring "import"
    names <- (parseUpperName `sepBy1` char '.') `sepBy1` char ','
    return $ Import names
  parseFromImport = do
    sstring "from"
    modName <- parseUpperName `sepBy1` char '.'
    sstring "import"
    names <- (parseUpperName <|> parseLowerName) `sepBy1` char ','
    return $ From modName names

parseStatement :: Parser Statement
parseStatement = try parseLetStmts
  <|> fmap Expression parseExpr

parseStatements :: Parser [Statement]
parseStatements = many1 $ do 
  stmt <- parseStatement
  schar ';'
  return stmt

test :: String -> IO ()
test input = case parse parseStatements "" input of
  Right val -> putStrLn $ "Parsed: " ++ show val
  Left err -> putStrLn $ "Error: " ++ show err