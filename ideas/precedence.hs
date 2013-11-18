import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))

data Expr =
  Number Double
  | Var String
  | Apply Expr Expr
  deriving (Show)

lexeme p = p <* spaces

pVar :: Parser Expr
pVar = lexeme $ Var <$> (many1 letter <|> (many1 $ oneOf "+-*/^"))

pNumber :: Parser Expr
pNumber = lexeme $ (Number . read) <$> many1 digit

pApply :: Parser Expr
pApply = chainl1 pExpr (pure Apply <* spaces)

pExpr = choice [pApply, pVar, pNumber]

test parser input = parse parser "" input
