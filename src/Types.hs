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
              getClasses, builtinFuncs,
              unionAll, apply, type_,
              tmUnion, tmInsert, tmSingle,
              tmElems, tmEmpty, tmLookup,
              tmDelete, (•),
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

data Polytype = Polytype [Name] Type deriving (Show, Eq, Ord)

builtinFuncs = S.fromList ["+", "-", "*", "/", ">", "<", ">=", "<=",
                           "==", "!=", "::", "[]", "(if)", "(or)",
                           "(error)", "(fail)", "(range)"]

-- | The Types class describes objects which can contain free type
-- variables, i.e. those which are not determined by their containers,
-- and things to which we can apply type substitutions
class Types a where
  free :: a -> S.Set String
  applySub :: Substitutions -> a -> a

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
  TVar classNames _ -> classNames
  TConst _ -> []
  TApply a b ->
    let classes = getClasses name a in
    if null classes then getClasses name b else classes
  a :=> b -> getClasses name (TApply a b)
  TTuple ts -> case ts ! map (getClasses name) ! filter (not . null) of
    [] -> []
    cs:_ -> cs


instance Types Polytype where
  free (Polytype vars t) = (free t) S.\\ (S.fromList vars)
  applySub s (Polytype vars t) =
    Polytype vars (applySub (foldr M.delete s vars) t)

instance Types a => Types [a] where
  free l = mconcat (map free l)
  applySub s = map (applySub s)

type TypeMap = M.Map Name Polytype

instance Types TypeMap where
  free env = free (M.elems env)
  applySub s env = applySub s <$> env

instance Monoid TypeMap where
  mempty = tmEmpty
  mappend = tmUnion

type Substitutions = M.Map String Type

(•) :: Substitutions -> Substitutions -> Substitutions
s1 • s2 = (applySub s1 <$> s2) `M.union` s1

instance Render Substitutions where
  render _ subs = rndr pairs
    where rndr [] = "{}"
          rndr [pair] = "{" ++ toS pair ++ "}"
          rndr pairs = "{\n" ++ (intercalate ",\n" $ map toS pairs) ++ "\n}"
          pairs = M.toList subs ! filter isNotBuiltIn
          toS (key, val) = "   " ++ key ++ " : " ++ render 0 val
          isNotBuiltIn = (\(name, _) -> not $ name `S.member` builtinFuncs)

instance Render TypeMap where
  render _ tmap = rndr pairs
    where rndr [] = "{}"
          rndr [pair] = "{" ++ toS pair ++ "}"
          rndr pairs = "{\n" ++ (intercalate ",\n" $ map toS pairs) ++ "\n}"
          pairs = M.toList tmap ! filter isNotBuiltIn
          toS (key, val) = "   " ++ key ++ " : " ++ render 0 val
          isNotBuiltIn = (\(name, _) -> not $ name `S.member` builtinFuncs)

instance Render Polytype where
  render n (Polytype [] t) = render n t
  render n (Polytype vars t) = "∀" ++ intercalate " " vars ++ ". " ++ render n t

instance Render () where
  render _ () = "()"

tmLookup name m = M.lookup name m
tmInsert name typ m = M.insert name typ m
tmUnion m1 m2 = M.union m1 m2
infixl 4 `tmUnion`
tmElems m = M.elems m
tmEmpty = M.empty
tmSingle name typ = M.singleton name typ
tmDelete name m = M.delete name m

apply :: M.Map Name Type -> Type -> Type
apply subs t@(TVar [] name) = M.findWithDefault t name subs
apply _    (TConst name) = TConst name
apply subs (TTuple ts)   = TTuple (apply subs <$> ts)
apply subs (TApply a b)  = TApply (apply subs a) (apply subs b)
apply subs (a :=> b)     = apply subs a :=> apply subs b

bare = Polytype []
barev = bare . TVar []
num  = TConst "Number"
str  = TConst "String"
bool = TConst "Bool"
tuple = TTuple

data Sig = TSig Name Type deriving (Show)
type Kind = Type

type_ :: Kind
type_ = TVar [] "(Type)"

data TypeClass = TC
  {
    kind     :: Kind
  , varName  :: Name
  , sigs     :: [Sig]
  , inherits :: [Name]
  } deriving (Show)
