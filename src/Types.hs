{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module Types (Type(..),
              Polytype(..),
              TypeMap(..),
              Types(..),
              Substitutions(..),
              unionAll, apply,
              tmUnion, tmInsert, tmSingle,
              tmElems, tmEmpty, tmLookup,
              tmDelete, (•), noSubstitutions,
              num, bool, str, tuple,
              bare, barev) where

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

data Polytype = Polytype [Name] Type deriving (Show, Eq, Ord)

-- | The Types class describes objects which can contain free type
-- variables, i.e. those which are not determined by their containers,
-- and things to which we can apply type substitutions
class Types a where
  free :: a -> S.Set String
  applySub :: Substitutions -> a -> a

instance Types Type where
  free (TVar name) = S.singleton name
  free (TConst _) = S.empty
  free (TTuple ts) = map free ts ! unionAll
  free (TApply t1 t2) = free t1 `S.union` free t2
  free (t1 :=> t2) = free t1 `S.union` free t2

  applySub s (TVar n) = case M.lookup n s of
    Nothing  -> TVar n
    Just t   -> t
  applySub s (TApply t1 t2) = applySub s t1 `TApply` applySub s t2
  applySub s (t1 :=> t2) = applySub s t1 :=> applySub s t2
  applySub s (TTuple ts) = TTuple $ applySub s <$> ts
  applySub _ t = t

instance Types Polytype where
  free (Polytype vars t) = (free t) S.\\ (S.fromList vars)
  applySub s (Polytype vars t) =
    Polytype vars (applySub (foldr M.delete s vars) t)

instance Types a => Types [a] where
  free l = mconcat (map free l)
  applySub s = map (applySub s)

newtype TypeMap = TM (M.Map Name Polytype) deriving (Show)

instance Types TypeMap where
  free (TM env) = free (M.elems env)
  applySub s (TM env) = TM (applySub s <$> env)

type Substitutions = M.Map String Type

noSubstitutions :: Substitutions
noSubstitutions = M.empty

(•) :: Substitutions -> Substitutions -> Substitutions
s1 • s2 = (applySub s1 <$> s2) `M.union` s1

instance Render Substitutions where
  render _ subs = "{\n" ++ (intercalate ",\n" $ map toS pairs) ++ "\n}"
    where pairs = M.toList subs
          toS (key, val) = "   " ++ key ++ " => " ++ render 0 val

instance Render TypeMap where
  render _ (TM env) = "{\n" ++ (intercalate ",\n" $ map toS pairs) ++ "\n}"
    where pairs = M.toList env ! filter (\(name, _) -> head name /= '(')
          toS (key, val) = "   " ++ key ++ " : " ++ render 0 val

instance Render Polytype where
  render n (Polytype [] t) = render n t
  render n (Polytype vars t) = "∀" ++ intercalate " " vars ++ ". " ++ render n t

instance Render () where
  render _ () = "()"

tmLookup name (TM m) = M.lookup name m
tmInsert name typ (TM m) = TM $ M.insert name typ m
tmUnion (TM m1) (TM m2) = TM $ M.union m1 m2
infixl 4 `tmUnion`
tmElems (TM m) = M.elems m
tmEmpty = TM M.empty
tmSingle name typ = TM $ M.singleton name typ
tmDelete name (TM m) = TM $ M.delete name m

apply :: M.Map Name Type -> Type -> Type
apply subs t@(TVar name) = M.findWithDefault t name subs
apply _    (TConst name) = TConst name
apply subs (TTuple ts)   = TTuple (apply subs <$> ts)
apply subs (TApply a b)  = TApply (apply subs a) (apply subs b)
apply subs (a :=> b)     = apply subs a :=> apply subs b

bare = Polytype []
barev = bare . TVar
num  = TConst "Number"
str  = TConst "String"
bool = TConst "Bool"
tuple = TTuple
