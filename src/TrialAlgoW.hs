module AlgorithmW where

type Name = String
data Expr =
  Var Name
  | Apply Expr Expr
  | Lambda Name Expr
  | Let Name Expr Expr
  deriving (Show, Eq, Ord)

-- Monotypes. A monotype always designates a particular type, in the sense
-- that it is equal only to itself and different from all others. In other
-- words, (TVar n) == (TVar m) if and only if n == m
data Type =
  TVar Name
  | TApp Name Type Type
  deriving (Show)

data Polytype =
  Mono Type
  | Forall Name Polytype
  deriving (Show)

