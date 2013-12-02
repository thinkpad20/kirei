module AST (Expr(..),
            ListLiteral(..),
            Constructor(..),
            Name,
            Matches,
            prettyExpr,
            InString(..),
            symsToVars,
            caseToLambda) where

import Common
import Types
import qualified Data.Map as M

type Matches = [(Expr, Expr)]

data Constructor = Constructor Name [Type] deriving (Show, Ord, Eq)

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
  | Lambda Expr Expr
  | List ListLiteral
  | Datatype Name [Name] [Constructor] (Maybe Expr)
  | Sig Name Type (Maybe Expr)
  deriving (Show, Eq, Ord)

data ListLiteral =
  ListLiteral [Expr]
  | ListRange Expr Expr
  deriving (Show, Eq, Ord)

prettyExpr :: Expr -> String
prettyExpr e = case e of
  Bool b -> show b
  Number n -> show n
  String s -> show s
  Symbol op -> op
  Var v -> v
  Underscore -> "_"
  If c t f -> "if " ++ prettyExpr c ++ " then " ++
                    prettyExpr t ++ " else " ++ prettyExpr f
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
  Lambda p e -> "\\" ++ prettyExpr p ++ " -> " ++ prettyExpr e
  List (ListLiteral es) -> "[" ++ intercalate ", " (prettyExpr <$> es) ++ "]"
  List (ListRange start stop) -> "[" ++ prettyExpr start ++ ".." ++
                                    prettyExpr stop ++ "]"
  Sig name typ e -> "sig " ++ name ++ " : " ++ show typ ++ "; " ++ (case e of
    Nothing -> ""
    Just e2 -> prettyExpr e2)

data InString =
  Plain String
  | InterShow InString Expr InString
  | Interpolate InString Expr InString

instance Show InString where
  show (Plain s) = show s
  show (InterShow s e s') = show s ++ " ++ show (" ++ show e ++ ") ++ " ++ show s'
  show (Interpolate s e s') = show s ++ " ++ (" ++ show e ++ ") ++ " ++ show s'

symsToVars :: Expr -> Expr
symsToVars expr = case expr of
  Symbol s -> Var s
  If c t f -> If (symsToVars c) (symsToVars t) (symsToVars f)
  Let name e Nothing -> Let name (symsToVars e) Nothing
  Let name e (Just e') -> Let name (symsToVars e) (Just (symsToVars e'))
  Apply a b -> Apply (symsToVars a) (symsToVars b)
  Dotted a b -> Dotted (symsToVars a) (symsToVars b)
  Comma a b -> Comma (symsToVars a) (symsToVars b)
  c@(Case e ms) -> symsToVars $ caseToLambda c
  Tuple es -> Tuple $ map symsToVars es
  Lambda pat e -> Lambda (symsToVars pat) (symsToVars e)
  List (ListLiteral l) -> List (ListLiteral $ map symsToVars l)
  List (ListRange a b) -> List (ListRange (symsToVars a) (symsToVars b))
  Datatype n ns cs (Just e) -> Datatype n ns cs (Just $ symsToVars e)
  e -> e

caseToLambda :: Expr -> Expr
caseToLambda expr = case expr of
  Case e ms -> Lambda e (compile e ms)
  where
    compile _ [] = Var "__matchError__"
    compile e ((p,r):ms) =
      Apply (Apply (Symbol "[-]") (Apply (Lambda p r) e)) (compile e ms)
