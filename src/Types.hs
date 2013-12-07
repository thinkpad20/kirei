{-# LANGUAGE NoMonomorphismRestriction #-}
module Types (Type(..),
              Scheme(..),
              Types(..),
              TypeMap(..),
              Substitutions,
              unionAll, initials,
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
  TypeVar Name            -- e.g. `a` in `foo : a -> (a, String)` or `List a`
  | NamedType Name [Type] -- e.g. `String` or `List Int`
  | Type :=> Type         -- functions
  deriving (Show, Eq, Ord)

infixr 4 :=>

instance Render Type where
  render _ t = case t of
    TypeVar name -> name
    NamedType "" ts -> "(" ++ intercalate ", " (map (render 0) ts) ++ ")"
    NamedType "List" ts -> "[" ++ intercalate ", " (map (render 0) ts) ++ "]"
    NamedType name [] -> name
    NamedType name ts -> name ++ " " ++ (intercalate " " $ map show' ts)
    t1 :=> t2 -> show' t1 ++ " -> " ++ render 0 t2
    where show' t@(NamedType "List" _) = render 0 t
          show' t@(NamedType (_:_) (_:_)) = "(" ++ render 0 t ++ ")"
          show' t@(a :=> b) = "(" ++ render 0 t ++ ")"
          show' t = render 0 t

instance Render Scheme where
  render _ s = show s

data Scheme = Scheme [Name] Type
type Substitutions = M.Map Name Type

class Types a where
  free :: a -> S.Set Name
  apply :: Substitutions -> a -> a

instance Types Type where
  free (TypeVar name) = S.singleton name
  free (NamedType name ts) = unionAll (free <$> ts)
  free (t1 :=> t2) = free t1 `S.union` free t2

  apply subs t@(TypeVar name) = M.findWithDefault t name subs
  apply subs (NamedType name ts) = NamedType name $ fmap (apply subs) ts
  apply subs (a :=> b) = apply subs a :=> apply subs b

instance Types Scheme where
  free (Scheme vars t) = (free t) S.\\ S.fromList vars
  apply subs (Scheme vars t) =
    Scheme vars (apply (foldr M.delete subs vars) t)

data TypeMap = TM (M.Map Name Scheme)

instance Types TypeMap where
  free (TM env) = unionAll (free <$> M.elems env)
  apply subs (TM env) = TM $ apply subs <$> env

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

bare = Scheme []
barev = var ~> bare
num  = NamedType "Number" []
str  = NamedType "String" []
bool = NamedType "Bool" []
tuple = NamedType ""
var = TypeVar
listT t = NamedType "List" [t]

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
    ("Just", witha $ a :=> NamedType "Maybe" [a]),
    ("Nothing", witha $ NamedType "Maybe" [a])
  ]
  where witha = Scheme ["a"]
        a = var "a"
        b = var "b"
