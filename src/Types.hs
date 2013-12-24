{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module Types (Type(..),
              Kind,
              TypeClass(..),
              Polytype(..),
              TypeMap(..),
              Types(..),
              Sig(..),
              Substitutions(..),
              renderMap, inherits,
              getClasses, builtinFuncs,
              unionAll, type_, (•), sig,
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
  TVar [Name] Name     -- First arg is class name(s)
  | TConst Name      -- e.g. `Number`
  | TApply Type Type -- e.g. `Maybe a` or `State [Number] a`
  | TTuple [Type] -- e.g. `(a, Number)`
  | Type :=> Type  -- functions
  deriving (Show, Eq, Ord)

data Polytype = Polytype [Name] Type deriving (Show, Eq, Ord)

data TypeClass = TC Type Kind [Sig] deriving (Show)

type Kind = Type

type Sig = (Name, Type)

type TypeMap = M.Map Name Polytype

type Substitutions = M.Map String Type

infixr 4 :=>

instance Render Type where
  render _ t = case t of
    TVar [] name -> name
    TVar cNames name -> "(" ++ name ++ " :~ " ++ int ", " cNames ++ ")"
    TConst name -> name
    TApply (TConst "[]") t -> "[" ++ r t ++ "]"
    TApply (TConst c) t -> c ++ " " ++ r t
    TApply (TVar [] v) t -> v ++ " " ++ r t
    TApply t1 t2 -> "(" ++ r t1 ++ " " ++ r t2 ++ ")"
    TTuple ts -> "(" ++ intercalate ", " (map r ts) ++ ")"
    t1@(a :=> b) :=> t2 -> "(" ++ r t1 ++ ")" ++ " -> " ++ r t2
    t1 :=> t2 -> r t1 ++ " -> " ++ r t2
    where r = render 0
          int = intercalate

-- | The Types class describes objects which can contain free type
-- variables, i.e. those which are not determined by their containers,
-- and things to which we can apply type substitutions
class Types a where
  free :: a -> S.Set String
  applySub :: Substitutions -> a -> a

instance Show Sig where
  show (name, typ) = name ++ " : " ++ show typ

instance Types Type where
  free (TVar _ name) = S.singleton name
  free (TConst _) = S.empty
  free (TTuple ts) = map free ts ! unionAll
  free (TApply t1 t2) = free t1 `S.union` free t2
  free (t1 :=> t2) = free t1 `S.union` free t2

  applySub s (TVar classes varName) = case M.lookup varName s of
    Nothing  -> TVar classes varName
    Just t   -> t
  applySub s (TApply t1 t2) = applySub s t1 `TApply` applySub s t2
  applySub s (t1 :=> t2) = applySub s t1 :=> applySub s t2
  applySub s (TTuple ts) = TTuple $ applySub s <$> ts
  applySub _ t = t

-- | @getClasses@ will search through a type for a type variable
-- and return whatever type classes are associated with that variable
getClasses :: Name -> Type -> [Name]
getClasses name typ = case typ of
  TVar classNames name' | name == name' -> classNames
  TApply a b ->
    let classes = getClasses name a in
    if null classes then getClasses name b else classes
  a :=> b -> getClasses name (TApply a b)
  TTuple ts -> case ts ! map (getClasses name) ! filter (not . null) of
    [] -> []
    cs:_ -> cs
  otherwise -> []

instance Types Polytype where
  free (Polytype vars t) = (free t) S.\\ (S.fromList vars)
  applySub s (Polytype vars t) =
    Polytype vars (applySub (foldr M.delete s vars) t)

instance Types a => Types [a] where
  free l = mconcat (map free l)
  applySub s = map (applySub s)

instance Types TypeMap where
  free env = free (M.elems env)
  applySub s env = applySub s <$> env

instance Render Substitutions where
  render _ subs = renderMap subs

instance Render TypeMap where
  render _ tmap = renderMap tmap

instance Render Polytype where
  render n (Polytype [] t) = render n t
  render n (Polytype vars t) = "∀" ++ intercalate " " vars ++ ". " ++ render n t

instance Render () where
  render _ () = "()"

-- Wrapper functions
bare :: Type -> Polytype
bare = Polytype []
barev :: Name -> Polytype
barev = bare . TVar []

num, str, bool :: Type
num  = TConst "Number"
str  = TConst "String"
bool = TConst "Bool"

tuple :: [Type] -> Type
tuple = TTuple

type_ :: Kind
type_ = TVar [] "(Type)"

sig :: Name -> Type -> Sig
sig name typ = (name, typ)

-- Convenience functions
renderMap :: Render a => M.Map Name a -> String
renderMap m = rndr pairs
  where rndr [] = "{}"
        rndr [(key, val)] = "{" ++ key ++ " : " ++ render 0 val ++ "}"
        rndr pairs = "{\n" ++ (intercalate ",\n" $ map toS pairs) ++ "\n}"
        pairs = M.toList m ! filter isNotBuiltIn
        toS (key, val) = "   " ++ key ++ " : " ++ render 0 val
        isNotBuiltIn = (\(name, _) -> not $ name `S.member` builtinFuncs)

(•) :: Substitutions -> Substitutions -> Substitutions
s1 • s2 = (applySub s1 <$> s2) `M.union` s1

inherits :: TypeClass -> [Name]
inherits (TC (TVar inherits' _) _ _) = inherits'
inherits tc = error $ "FATAL: malformed type class `" ++ show tc ++ "`"

builtinFuncs = S.fromList [  "+", "-", "*", "/", ">"
                           , "<", ">=", "<=", "=="
                           , "!=", "::", "[]", "(if)", "(or)"
                           , "(error)", "(fail)", "(range)"]
