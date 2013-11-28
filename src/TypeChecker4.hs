{-# LANGUAGE LambdaCase #-}
module TypeChecker where

import Control.Monad.State hiding (get, modify)
import qualified Control.Monad.State as MS (get, modify)
import qualified Data.Map as M
import qualified Data.Set as S
import AST
import Parser
import Common

-- whatever fields we want to have in our state
data Attribute =
  TypeMap TMap
  | UsedNames UNames
  | Subs Substitutions

type TMap = M.Map Name Polytype
type UNames = S.Set Name
type Substitutions = M.Map Name Type
type Env = M.Map Name Attribute

data Type =
  TypeVar Name -- e.g. `a` in `foo : a -> (a, String)` or `List a`
  | NamedType Name [Type] -- e.g. `String` or `List Int`
  | Type :=> Type -- functions
  deriving (Show)

data Polytype = Polytype [Name] Type
type TypeChecker = StateT Env IO


num  = NamedType "Number" []
str  = NamedType "String" []
bool = NamedType "Bool" []

class Types a where
  free :: a -> S.Set Name
  substitute :: Substitutions -> a -> a

instance Types Type where
  free (TypeVar name) = S.singleton name
  free (NamedType _ ts) = foldr S.union S.empty (free <$> ts)
  free (t1 :=> t2) = free t1 `S.union` free t2

  substitute subs t@(TypeVar name) = M.findWithDefault t name subs
  substitute subs (t1 :=> t2) = substitute subs t1 :=> substitute subs t2
  substitute subs (NamedType nm ts) = NamedType nm (substitute subs <$> ts)

instance Types Polytype where
  -- free variables in a polytype are any free variables in its term, minus
  -- any variables bound in its name list
  free (Polytype names t) = free t `S.difference` S.fromList names

  -- substituting in a polytype means replacing all of the *bound* variables
  -- in the *term* that appear in the substitution, with their substitutes
  substitute subs (Polytype names term) =
    Polytype names (foldr M.delete subs names `substitute` term)

------------------------------------------------------------------------------
----                          GETTERS AND SETTERS                         ----
------------------------------------------------------------------------------

get :: Name -> TypeChecker Attribute
get attrName = do
  env <- MS.get
  case M.lookup attrName env of
    Just attribute -> return attribute
    Nothing -> error $ "Error: invalid attribute `" ++ show attrName ++ "`"

set :: Name -> Attribute -> TypeChecker ()
set attrName attribute = do
  env <- MS.get
  put $ M.insert attrName attribute env

modify :: Name -> (Attribute -> Attribute) -> TypeChecker ()
modify attrName f = get attrName >>= f ~> set attrName

setSubs :: Attribute -> TypeChecker ()
setSubs = set "subs"
getSubs :: TypeChecker Attribute
getSubs = get "subs"
modSubs :: (Attribute -> Attribute) -> TypeChecker ()
modSubs f = modify "subs" f

setTM :: Attribute -> TypeChecker ()
setTM = set "typemap"
getTM :: TypeChecker Attribute
getTM = get "typemap"
modTM :: (Attribute -> Attribute) -> TypeChecker ()
modTM f = modify "typemap" f

setUsed :: Attribute -> TypeChecker ()
setUsed = set "used"
getUsed :: TypeChecker Attribute
getUsed = get "used"
modUsed :: (Attribute -> Attribute) -> TypeChecker ()
modUsed f = modify "used" f

with :: (Env -> Env) -> TypeChecker a -> TypeChecker a
with f action1 = do
  env <- MS.get
  put $ f env
  result <- action1
  put env
  return result


lookupTM name = getTM >>= \(TypeMap tm) -> return $ M.lookup name tm
checkUsed name = getUsed >>= \(UsedNames u) -> return $ S.member name u
addUsed name = modUsed (\(UsedNames u) -> UsedNames $ S.insert name u)
removeUsed name = modUsed (\(UsedNames u) -> UsedNames $ S.delete name u)
addSub name typ = modSubs (\(Subs s) -> Subs $ M.insert name typ s)
makeTMSubs = do
  Subs subs <- getSubs
  modTM (\(TypeMap m) -> TypeMap $ substitute subs <$> m)

------------------------------------------------------------------------------
----                          TYPE INFERRENCE                             ----
------------------------------------------------------------------------------


infer :: Expr -> TypeChecker Type
infer expr = case expr of
  Number _ -> return num
  String _ -> return str
  Bool   _ -> return bool
  Var name -> lookupTM name >>= \case
    Nothing -> error $ "Undefined name `" ++ name ++ "`"
    Just sigma -> instantiate sigma
  Apply e0 e1 -> do
    t0 <- infer e0
    t1 <- infer e1
    t' <- newvar
    unify (t1, t0 :=> t1)
  Let name e0 e1 -> do
    t <- infer e0
    case e1 of
      Nothing -> return $ NamedType "" []
      Just e1 -> with (fmap $ setTM' name $ generalize t) (infer e1)


  where
    newvar = makename "a"
    makename name = checkUsed name >>= \case
      False -> addUsed name >> pure (TypeVar name)
      True -> let (c:cs) = reverse name in
        makename $ reverse (if c < 'z' then succ c:cs else 'a':c:cs)
    instantiate (Polytype names term) = do
      newNames <- mapM makename names
      return $ substitute (M.fromList $ zip names newNames) term
    unify (TypeVar a, t) = addSub a t >> makeTMSubs >> pure t
    unify (t, TypeVar a) = unify (TypeVar a, t)
    unify (a :=> b, c :=> d) = unify (a,b) >> makeTMSubs >> unify (c,d)
    generalize = undefined
    setTM' name t attr = case attr of
      TypeMap m -> TypeMap $ M.insert name t m
      _ -> attr
