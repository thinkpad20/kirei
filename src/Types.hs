module Types (Type(..),
              Scheme(..),
              Types(..),
              TypeEnvironment(..),
              Substitutions) where

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
  deriving (Eq, Ord)

infixr 4 :=>

instance Show Type where
  show t = case t of
    TypeVar name -> name
    NamedType "" ts -> "(" ++ intercalate ", " (map show ts) ++ ")"
    NamedType name [] -> name
    NamedType name ts -> name ++ " " ++ (intercalate " " $ map show' ts)
    t1 :=> t2 -> show' t1 ++ " -> " ++ show t2
    where show' t@(NamedType (_:_) (_:_)) = "(" ++ show t ++ ")"
          show' t@(a :=> b) = "(" ++ show t ++ ")"
          show' t = show t

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

data TypeEnvironment = TE (M.Map Name Scheme)

instance Types TypeEnvironment where
  free (TE env) = unionAll (free <$> M.elems env)
  apply subs (TE env) = TE $ apply subs <$> env

instance Show TypeEnvironment where
  show (TE env) = let
    pairs = M.toList env
    toS (key, val) = show key ++ " => " ++ show val
    in "{" ++ (intercalate ", " $ map toS pairs) ++ "}"

instance Show Scheme where
  show (Scheme vars t) = loop vars where
    loop [] = show t
    loop (v:vs) = "∀" ++ v ++ "." ++ loop vs


unionAll = foldl' S.union S.empty
