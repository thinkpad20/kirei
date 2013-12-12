module AST (Expr(..),
            ListLiteral(..),
            Constructor(..),
            Name,
            Matches,
            prettyExpr,
            InString(..),
            desugar,
            adtToSigs) where

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
  Symbol op -> op
  Var v -> v
  TypeName name -> name
  Underscore -> "_"
  If c t f -> "if " ++ prettyExpr c ++ " then " ++
                    prettyExpr t ++ " else " ++ prettyExpr f
  Let n e1 e2 -> "let " ++ n ++ " = " ++ prettyExpr e1 ++ "; " ++ handle e2
  Apply a b -> prettyExpr a ++ " " ++ prettyExpr b
  Dotted a b -> prettyExpr a ++ "." ++ prettyExpr b
  Comma a b -> prettyExpr a ++ ", " ++ prettyExpr b
  Case e matches -> "case " ++ prettyExpr e ++ " of " ++ sh matches where
    sh = map s ~> int "|"
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

symsToVars :: Expr -> Expr
symsToVars expr = case expr of
  Symbol s -> Var s
  If c t f -> If (rec c) (rec t) (rec f)
  Let name e Nothing -> Let name (rec e) Nothing
  Let name e (Just e') -> Let name (rec e) (Just (rec e'))
  Apply a b -> Apply (rec a) (rec b)
  Dotted a b -> Dotted (rec a) (rec b)
  Comma a b -> Comma (rec a) (rec b)
  c@(Case e ms) -> rec $ caseToLambda c
  Tuple es -> Tuple $ map rec es
  Lambda pat e -> Lambda (rec pat) (rec e)
  List (ListLiteral l) -> List (ListLiteral $ map rec l)
  List (ListRange a b) -> List (ListRange (rec a) (rec b))
  Datatype n ns cs (Just e) -> Datatype n ns cs (Just $ rec e)
  Sig name typ (Just next) -> Sig name typ (Just $ rec next)
  e -> e
  where rec = symsToVars

caseToLambda :: Expr -> Expr
caseToLambda expr = case expr of
  Case e ms -> compile e ms
  where
    compile _ [] = Var "__matchError__"
    compile e ((p,r):ms) =
      Apply (Apply (Var "__matchOr__") (Apply (Lambda p r) e)) (compile e ms)

{-
step :: Expr -> Expr -> Expr
step foo bar = Apply (Apply (Apply (::)) foo) bar
-}

desugarList :: Expr -> Expr
desugarList expr = case expr of
  List lit -> case lit of
    ListLiteral es -> foldr cons (TypeName "[]") es where
      cons a b = Apply (Apply (TypeName "::") a) b
    ListRange start stop -> Apply (Apply (Var "__listRange__") start) stop
  If c t f -> If (rec c) (rec t) (rec f)
  Apply a b -> Apply (rec a) (rec b)
  Comma a b -> Comma (rec a) (rec b)
  Dotted a b -> Dotted (rec a) (rec b)
  Case e ms -> Case (rec e) (map (\(a, b) -> (rec a, rec b)) ms)
  Lambda pat e -> Lambda (rec pat) (rec e)
  Tuple es -> Tuple (map rec es)
  Let name e Nothing -> Let name (rec e) Nothing
  Let name e1 (Just e2) -> Let name (rec e1) (Just $ rec e2)
  ADT n ns cs (Just e) -> adtToSigs (ADT n ns cs (Just $ rec e))
  ADT n ns cs Nothing -> adtToSigs expr
  Sig n t (Just e) -> Sig n t (Just $ rec e)
  _ -> expr
  where rec = desugarList

-- | adtToSigs makes a bunch of sig statements and creates a new Type which
-- should be added to the environment
adtToSigs :: Expr -> Expr
adtToSigs (ADT name vars constructors next) = makeSigs constructors where
  newType = foldl' TApply (TConst name) (TVar <$> vars)
  makeSigs (c:cs) = toSig c $ rest cs
  toSig (Constructor n ts) = Sig n (foldr (:=>) newType ts)
  rest [] = next
  rest (c:cs) = Just $ toSig c $ rest cs


desugar :: Expr -> Expr
desugar = desugarList ~> symsToVars
