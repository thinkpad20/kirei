module AST (Expr(..),
            ListLiteral(..),
            Constructor(..),
            Name,
            Matches,
            prettyExpr,
            InString(..),
            desugar) where

import Common
import Types
import Prelude hiding (foldr)
import Data.Char (isUpper)
import qualified Data.Map as M

type Matches = [(Expr, Expr)]

data Constructor = Constructor Name [Type] deriving (Show, Ord, Eq)

data Expr =
  Bool Bool
  | Number Double
  | String String
  | Symbol Name
  | TypeName Name
  | Var Name
  | Placeholder
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
  | ADT Name [Name] [Constructor] (Maybe Expr)
  | Sig Name Type (Maybe Expr)
  deriving (Show, Eq, Ord)

data ListLiteral =
  ListLiteral [Expr]
  | ListRange Expr Expr
  deriving (Show, Eq, Ord)

instance Render Expr where
  render _ = prettyExpr

instance Render Constructor where
  render n (Constructor name []) = name
  render n (Constructor name ts) = name ++ " " ++ int " " (render n <$> ts)
    where int = intercalate

prettyExpr :: Expr -> String
prettyExpr e = case e of
  Bool b -> show b
  Number n -> if isInt n then show $ floor n else show n
  String s -> show s
  Symbol op -> "(" ++ op ++ ")"
  Var v -> v
  TypeName name -> name
  Placeholder -> "?"
  If c t f -> "if " ++ prettyExpr c ++ " then " ++
                    prettyExpr t ++ " else " ++ prettyExpr f
  Let n e1 e2 -> "let " ++ n ++ " = " ++ prettyExpr e1 ++ "; " ++ handle e2
  Apply a@(Apply _ _) b -> "(" ++ prettyExpr a ++ " " ++ prettyExpr b ++ ")"
  Apply a b -> prettyExpr a ++ " " ++ prettyExpr b
  Dotted a b -> prettyExpr a ++ "." ++ prettyExpr b
  Comma a b -> prettyExpr a ++ ", " ++ prettyExpr b
  Case e matches -> "case " ++ prettyExpr e ++ " of " ++ sh matches where
    sh = map s ~> int " | "
    s (ex, exs) = prettyExpr ex ++ " -> " ++ prettyExpr exs
  Tuple es -> "(" ++ int ", " (prettyExpr <$> es) ++ ")"
  Lambda p e -> "λ" ++ prettyExpr p ++ " -> " ++ prettyExpr e
  List (ListLiteral es) -> "[" ++ int ", " (prettyExpr <$> es) ++ "]"
  List (ListRange start stop) -> "[" ++ prettyExpr start ++ ".." ++
                                    prettyExpr stop ++ "]"
  Sig name typ e -> "sig " ++ name ++ " : " ++ render 0 typ ++ "; " ++ handle e
  ADT name vars cs next -> "adt " ++ name ++ " " ++ int " " vars ++ " = " ++
    int " | " (map (render 0) cs) ++ "; " ++ handle next
  where handle next = case next of Nothing -> ""
                                   Just e -> prettyExpr e
        int = intercalate

data InString =
  Plain String
  | InterShow InString Expr InString
  | Interpolate InString Expr InString

instance Show InString where
  show (Plain s) = show s
  show (InterShow s e s') = show s ++ " ++ show (" ++ show e ++ ") ++ " ++ show s'
  show (Interpolate s e s') = show s ++ " ++ (" ++ show e ++ ") ++ " ++ show s'

desugar :: Expr -> Expr
desugar expr = case expr of
  Symbol s -> Var s
  If c t f -> Apply (Apply (Apply (Var "(if)") (rec c)) (rec t)) (rec f)
  List lit -> case lit of
    ListLiteral es -> foldr cons (TypeName "[]") es where
      cons a b = Apply (Apply (TypeName "::") (rec a)) (rec b)
    ListRange start stop -> Apply (Apply (Var "(range)") (rec start)) (rec stop)
  Apply a b -> Apply (rec a) (rec b)
  Comma a b -> Comma (rec a) (rec b)
  Dotted a b -> Dotted (rec a) (rec b)
  Case expr' matches -> caseToLambda (rec expr') matches
  Lambda pat e -> Lambda (rec pat) (rec e)
  Tuple es -> Tuple (map rec es)
  Let name e Nothing -> Let name (rec e) Nothing
  Let name e1 (Just e2) -> Let name (rec e1) (Just $ rec e2)
  ADT n ns cs (Just e) -> ADT n ns cs (Just $ rec e)
  Sig n t (Just e) -> Sig n t (Just $ rec e)
  otherwise -> expr
  where
    -- shorthand for recursing
    rec = desugar
    -- the goal here is to string together pattern lambdas with (or), with
    -- the last function returned an `(error)` indicating the match failed.
    -- see SPJ's book for more info.
    caseToLambda :: Expr -> [(Expr, Expr)] -> Expr
    --caseToLambda _ [] = Var "(error)"
    --caseToLambda e ((pat,res):ms) =
    --  Apply (Apply (Var "(or)") (Apply (Lambda pat (rec res)) e)) (rec e ms)
    caseToLambda expr matches = foldr mkLambda (Var "(error)") matches where
      mkLambda (pattern, result) =
        Apply (Apply (Var "(or)") (Apply (Lambda pattern (rec result)) expr))
    -- | adtToSigs makes a bunch of sig statements and creates a new Type which
    -- should be added to the environment
