module AST (Expr(..),
            ListLiteral(..),
            Constructor(..),
            TypeName(..),
            Name,
            Matches,
            prettyExpr) where

import Common
import qualified Data.Map as M

type Matches = [(Expr, Expr)]

data TypeName = TypeName Name [TypeName] deriving (Show, Ord, Eq)

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
  | Sig Name TypeName
  deriving (Show, Eq, Ord)

data ListLiteral =
  ListLiteral [Expr]
  | ListRange Expr Expr
  deriving (Show, Eq, Ord)

prettyExpr e = case e of
  Bool b -> show b
  Number n -> show n
  String s -> show s
  Symbol op -> op
  Var v -> v
  Underscore -> "_"
  If c t f -> "if " ++ prettyExpr c ++ " then " ++ prettyExpr t ++ " else " ++ prettyExpr f
  Let n e1 e2 -> "let " ++ n ++ " = " ++ prettyExpr e1 ++ "; " ++ (case e2 of
    Nothing -> ""
    Just e2 -> prettyExpr e2)
  Apply a b -> prettyExpr a ++ " " ++ prettyExpr b
  Dotted a b -> prettyExpr a ++ "." ++ prettyExpr b
  Comma a b -> prettyExpr a ++ ", " ++ prettyExpr b
  Case e matches -> "case " ++ prettyExpr e ++ " of " ++ sh matches where
    sh = map s ~> intercalate "|"
    s (ex, exs) = prettyExpr ex ++ " -> " ++ prettyExpr exs
  Tuple es -> "(" ++ intercalate ", " (prettyExpr <$> es) ++ ")"
  Lambda n e -> "\\" ++ n ++ " -> " ++ prettyExpr e
  List (ListLiteral es) -> "[" ++ intercalate ", " (prettyExpr <$> es) ++ "]"
  List (ListRange start stop) -> "[" ++ prettyExpr start ++ ".." ++ prettyExpr stop ++ "]"
