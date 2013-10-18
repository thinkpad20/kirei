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
  io <- optionMaybe $ string "io"
  funcName <- pIdentifier
  schar ':'
  sig <- pTypeSig
  return $ Sig funcName (isJust io) sig

test :: String -> Either ParseError Sig
test str = parse pSig "" str