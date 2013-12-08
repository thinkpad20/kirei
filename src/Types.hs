{-# LANGUAGE NoMonomorphismRestriction #-}
module Types (Type(..),
              Scheme(..),
              FreeVars(..),
              TypeMap(..),
              unionAll, initials, apply,
              tmUnion, tmInsert, tmSingle,
              tmElems, tmEmpty, tmLookup,
              num, bool, str, tuple,
              listT, var, bare, barev) where

import Prelude hiding (foldr)
import Common
import qualified Data.Map as M
import qualified Data.Set as S

-------------------------------------------------------
----  Data types for Hindley-Milner Type Checker   ----
-------------------------------------------------------
data Type =
  TVar Name        -- e.g. `a` in `foo : a -> (a, String)` or `List a`
  | TConst Name      -- e.g. `Number`
  | TApply Type Type -- e.g. `Maybe a` or `State [Number] a`
  | TTuple [Type] -- e.g. `(a, Number)`
  | Type :=> Type  -- functions
  deriving (Show, Eq, Ord)

infixr 4 :=>

instance Render Type where
  render _ t = case t of
    TVar name -> name
    TConst name -> name
    TApply (TConst "[]") t -> "[" ++ r t ++ "]"
    TApply (TConst c) t -> c ++ " " ++ r t
    TApply (TVar v) t -> v ++ " " ++ r t
    TApply t1 t2 -> "(" ++ r t1 ++ " " ++ r t2 ++ ")"
    TTuple ts -> "(" ++ intercalate ", " (map r ts) ++ ")"
    t1@(a :=> b) :=> t2 -> "(" ++ r t1 ++ ")" ++ " -> " ++ r t2
    t1 :=> t2 -> r t1 ++ " -> " ++ r t2
    where r = render 0

instance Render Scheme where
  render _ = show

data Scheme = Scheme [Name] Type deriving (Eq, Ord)

class FreeVars a where
  free :: a -> S.Set Name

instance FreeVars Type where
  free (TVar name) = S.singleton name
  free (TConst _) = S.empty
  free (TTuple ts) = map free ts ! unionAll
  free (TApply t1 t2) = free t1 `S.union` free t2
  free (t1 :=> t2) = free t1 `S.union` free t2

instance FreeVars Scheme where
  free (Scheme vars t) = (free t) S.\\ S.fromList vars

data TypeMap = TM (M.Map Name Scheme)

instance FreeVars TypeMap where
  free (TM env) = free <$> env ! unionAll

instance Show TypeMap where
  show (TM env) = "{" ++ (intercalate ", " $ map toS pairs) ++ "}"
    where pairs = M.toList env
          toS (key, val) = key ++ ": " ++ render 0 val

instance Render TypeMap where
  render _ (TM env) = "{\n" ++ (intercalate ",\n" $ map toS pairs) ++ "\n}"
    where pairs = M.toList env
          toS (key, val) = "   " ++ key ++ " : " ++ render 0 val

instance Show Scheme where
  show (Scheme vars t) = loop vars where
    loop [] = render 0 t
    loop (v:vs) = "∀" ++ v ++ "." ++ loop vs

instance Render () where
  render _ () = "()"

unionAll = foldl' S.union S.empty
tmLookup name (TM m) = M.lookup name m
tmInsert name typ (TM m) = TM $ M.insert name typ m
tmUnion (TM m1) (TM m2) = TM $ M.union m1 m2
infixl 4 `tmUnion`
tmElems (TM m) = M.elems m
tmEmpty = TM M.empty
tmSingle name typ = TM $ M.singleton name typ

apply :: M.Map Name Type -> Type -> Type
apply subs t@(TVar name) = M.findWithDefault t name subs
apply _    (TConst name) = TConst name
apply subs (TTuple ts)   = TTuple (apply subs <$> ts)
apply subs (TApply a b)  = TApply (apply subs a) (apply subs b)
apply subs (a :=> b)     = apply subs a :=> apply subs b

bare = Scheme []
barev = bare . var
num  = TConst "Number"
str  = TConst "String"
bool = TConst "Bool"
tuple = TTuple
var = TVar
listT = TApply (TConst "[]")
maybeT = TApply (TConst "Maybe")

initials = TM $ M.fromList
  [
    ("+", bare $ num :=> num :=> num),
    ("-", bare $ num :=> num :=> num),
    ("*", bare $ num :=> num :=> num),
    ("/", bare $ num :=> num :=> num),
    ("<", bare $ num :=> num :=> bool),
    (">", bare $ num :=> num :=> bool),
    ("<=", bare $ num :=> num :=> bool),
    (">=", bare $ num :=> num :=> bool),
    ("&&", bare $ bool :=> bool :=> bool),
    ("not", bare $ bool :=> bool),
    ("__matchFail__", witha a),
    ("__matchError__", witha a),
    ("[-]", witha $ a :=> a :=> a),
    ("Empty", witha $ listT a),
    ("::", witha $ a :=> listT a :=> listT a),
    ("__listRange__", witha $ a :=> a :=> listT a),
    ("undefined", witha a),
    ("map", Scheme ["a", "b"] ((a :=> b) :=> listT a :=> listT b)),
    ("one23", bare $ listT num),
    ("Just", witha $ a :=> maybeT a),
    ("Nothing", witha $ maybeT a),
    ("show", witha $ a :=> str),
    ("++", bare $ str :=> str :=> str)
  ]
  where witha = Scheme ["a"]
        a = var "a"
        b = var "b"
