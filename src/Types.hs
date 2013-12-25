{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module Types ( Type(..)
              , Kind
              , TypeClass(..)
              , Polytype(..)
              , TypeMap(..)
              , Types(..)
              , Sig(..)
              , Substitutions(..)
              , Instance(..)
              , Inferrer(..)
              , InferrerState(..)
              , TypeRecord(..)
              , TypeCheckError(..)
              , defaultInstances, defaultTypeMap
              , renderMap, inherits, getClasses, builtinFuncs
              , unionAll, type_, (•), sig, num, bool, str, tuple
              , bare, barev) where

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

type TypeMap = M.Map Name Polytype

type Substitutions = M.Map String Type

type Kind = Type

data TypeClass = TC Type Kind [Sig] deriving (Show)

type Sig = (Name, Type)

type Instance = Type

-- Type checker machinery, shared by TypeChecker and TypeClass

type Inferrer = ErrorT String (StateT InferrerState IO)
type TypeCheckError = String

data TypeRecord = Checked Polytype
                | Declared Type
                deriving (Show)

type Record = M.Map Name TypeRecord

instance Render Record where
  render _ rec = renderMap rec

instance Render TypeRecord where
  render _ (Checked ptype) = render 0 ptype
  render _ (Declared _type) = render 0 _type

data InferrerState =
  InferrerState { inferSupply :: Name
                , nameStack   :: [Name]
                , records   :: Record
                , kinds :: M.Map Name Kind
                , typeClasses :: M.Map Name TypeClass
                , instances :: M.Map Name (S.Set Type)
                } deriving (Show)

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
  render _ = renderMap

instance Render TypeMap where
  render _ = renderMap

instance Render (M.Map Name TypeClass) where
  render _ = renderMap

instance Render TypeClass where
  render _ = show

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

-- Default/initial versions of type checking data

defaultTypeMap = M.fromList
  [
    ("+", witha $ numa :=> numa :=> numa)
  , ("-", witha $ numa :=> numa :=> numa)
  , ("*", witha $ numa :=> numa :=> numa)
  , ("/", witha $ numa :=> numa :=> numa)
  , ("<", witha $ compa :=> compa :=> bool)
  , (">", witha $ compa :=> compa :=> bool)
  , ("<=", witha $ compa :=> compa :=> bool)
  , (">=", witha $ compa :=> compa :=> bool)
  , ("==", witha $ eqa :=> eqa :=> bool)
  , ("!=", witha $ eqa :=> eqa :=> bool)
  , ("(if)", witha $ bool :=> a :=> a :=> a)
  , ("[]", witha $ listT a)
  , ("::", witha $ a :=> listT a :=> listT a)
  , ("(fail)", witha a)
  , ("(error)", witha a)
  , ("(or)", witha $ a :=> a :=> a)
  , ("(range)", witha $ a :=> a :=> listT a)
  , ("show", witha $ a' ["Show"] :=> str)
  , ("map", withab $ (a :=> b) :=> f a :=> f b)
  ]
  where witha = Polytype ["a"]
        withab = Polytype ["a", "b"]
        a = TVar [] "a"
        b = TVar [] "b"
        a' classes = TVar classes "a"
        f = TApply (TVar ["Functor"] "f")
        listT = TApply (TConst "[]")
        maybeT = TApply (TConst "Maybe")
        numa = TVar ["Num"] "a"
        compa = TVar ["Comp"] "a"
        eqa = TVar ["Eq"] "a"

defaultInstances :: M.Map Name (S.Set Instance)
defaultInstances = M.fromList
  [
    ("Num",         S.fromList [num])
  , ("Eq",          S.fromList [ num
                               , str
                               , bool
                               , listOf (a ["Eq"])
                               ])
  , ("Show",        S.fromList [ num
                               , str
                               , bool
                               , listOf (a ["Show"])
                               ])
  , ("Monoid",      S.fromList [ num
                               , listOf (a [])])
  , ("Comp",       S.fromList [ num
                               , str
                               , listOf (a ["Comp"])])
  , ("Applicative", S.fromList [list])
  , ("Functor",     S.fromList [list])
  ]
  where list = TConst "[]"
        listOf = TApply list
        a classes = TVar classes "a"
