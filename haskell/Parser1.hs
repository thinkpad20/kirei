module Parser where

import Text.ParserCombinators.Parsec
import AST
import Data.Maybe

sstring s = spaces >> string s
schar c = spaces >> char c

pIdentifier :: Parser String
pIdentifier = spaces >> many1 letter

pSingleName :: Parser TypeSig
pSingleName = spaces >> do
  name <- pIdentifier
  return $ TypeName name

pTypeName :: Parser TypeSig
pTypeName = try pSingleName
  <|> do schar '('
         sigs <- pTypeSig
         schar ')'
         return sigs

pTypeSig :: Parser TypeSig
pTypeSig = pTypeName `chainl1` (sstring "->" >> return MapFrom)

pSig :: Parser Sig
pSig = do
  sstring "sig" >> spaces
  funcName <- pIdentifier
  schar ':'
  sig <- pTypeSig
  return $ Sig funcName sig

pDouble :: Parser Double
pDouble = do
  ds <- many1 digit
  dot <- optionMaybe $ char '.'
  case dot of
    Nothing -> return $ read ds
    _ -> do
      ds' <- many1 digit
      return $ read (ds ++ "." ++ ds')

pTerm :: Parser Term
pTerm = fmap Number pDouble <|> fmap Name pIdentifier

pSymbol :: Parser String
pSymbol = many1 $ oneOf "+-*/><=&?^~"

pSymTerm :: Parser Term
pSymTerm = spaces >> fmap Name pSymbol

---- Listed from lowest to highest precedence. Higher prec will be parsed first.
--pSymbolic = binary [pSymbol] logOr
--logOr = binary [string "||"] logAnd
--logAnd = binary [string "&&"] comparative
--comparative = binary (map (try.string) ["<=", ">=", "<", ">", "==", "!="]) additive
--additive = binary (map string ["+", "-"]) multiplicative
--multiplicative = binary (map string ["*", "/", "%"]) exponential
--exponential = binary [string "^"] unary

--binary :: [Parser String] -> Parser Expr -> Parser Expr
---- takes a list of operator parsers (ps) and the next higher-precedence rule,
---- parses a binary expression using one of the operators in the list.
--binary ps next = chainl1 next getOp
--  where getOp = do spaces
--                   s <- foldl1 (<|>) ps -- OR all of the parser rules in the list
--                   spaces
--                   return $ Binary s

pExpr :: Parser Expr
pExpr = (fmap Term pTerm) `chainl1` (sstring "+" >> spaces >> return Apply)

test :: String -> Either ParseError Expr
test str = parse pExpr "" str