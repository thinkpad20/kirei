{-# LANGUAGE NoMonomorphismRestriction #-}
module Types (Type(..),
              Polytype(..),
              FreeVars(..),
              TypeMap(..),
              unionAll, apply,
              tmUnion, tmInsert, tmSingle,
              tmElems, tmEmpty, tmLookup,
              tmDelete,
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

-- | The FreeVars class describes objects which can contain free type
-- variables, i.e. those which are not determined by their containers.
class FreeVars a where
  free :: a -> S.Set Name

instance FreeVars Type where
  free (TVar name) = S.singleton name
  free (TConst _) = S.empty
  free (TTuple ts) = map free ts ! unionAll
  free (TApply t1 t2) = free t1 `S.union` free t2
  free (t1 :=> t2) = free t1 `S.union` free t2

instance FreeVars Polytype where
  free (Polytype vars t) = (free t) S.\\ S.fromList vars

data TypeMap = TM (M.Map Name Polytype)

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

instance Render Polytype where
  render n (Polytype [] t) = render n t
  render n (Polytype vars t) = "∀" ++ intercalate " " vars ++ ". " ++ render n t

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
