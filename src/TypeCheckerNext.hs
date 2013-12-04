{-# LANGUAGE LambdaCase #-}
module TypeChecker where

import Control.Monad.State
import Data.Monoid
import Common
import Prelude hiding (foldr)
import Data.Foldable (foldr)
import Types
import AST
import Parser
import qualified Data.Map as M
import qualified Data.Set as S

type TypeChecker = StateT [(TypeMap, Restrictions)] IO
type BoundVars = S.Set Name
newtype Restrictions = Rs (M.Map Name Type) deriving (Show)

instance Render Restrictions where
  render _ = show

getRestrictions :: TypeChecker Restrictions
getRestrictions = get >>= \((_, rs):_) -> return rs

addRestriction :: Name -> Type -> TypeChecker ()
addRestriction name t = do
  (tms, Rs rs):rest <- get
  refined <- refineType t
  if name `S.member` getVars refined then
    error $ "Cycle in types: `" ++ name ++ "` to `" ++
      render 0 t ++ "` a.k.a. `" ++ render 0 refined ++ "`"
    else put ((tms, Rs $ M.insert name t rs):rest)

getTypeMap :: TypeChecker TypeMap
getTypeMap = get >>= \((tm, _):_) -> return tm

addMapping :: Name -> Type -> TypeChecker ()
addMapping name typ = do
  (TM mp, rs): rest <- get
  put $ (TM $ M.insert name typ mp, rs) : rest

pushEnv :: TypeChecker ()
pushEnv = get >>= \env -> put ((TM M.empty, Rs $ M.empty):env)

popEnv :: TypeChecker ()
popEnv = get >>= \(_:env) -> put env

isUsed :: Name -> TypeChecker Bool
isUsed name = getTypeMap >>= \(TM mp) -> return (name `M.member` mp)

infer :: Expr -> TypeChecker Type
infer expr = case expr of
  Number _ -> return num
  String _ -> return str
  Bool   _ -> return bool
  Var var  -> getTypeOf var
  Lambda (Var name) e -> do
    pushWithNew name
    resultT <- infer e
    paramT <- getTypeOf name >>= refineType
    popEnv
    return (paramT :=> resultT)
  Apply a b -> do
    aType <- infer a
    bType <- infer b
    case aType of
      (t1 :=> t2) -> unify bType t1 >> return t2
      (TypeVar _) -> do
        v <- newvar
        unify aType (bType :=> v)
        return v
      otherwise   -> error $ show a ++ " is not a function"
  Let name e next -> do
    pushWithNew name
    env <- get
    prnt $ "just pushed a new env " ++ show env
    eType <- infer e
    env <- get
    prnt $ "about to pop env " ++ show env
    popEnv
    env <- get
    prnt $ "popped env, env is now " ++ show env
    addMapping name eType
    case next of
      Nothing -> return $ tuple []
      Just next -> infer next
  where
    pushWithNew name = pushEnv >> newvar >>= addMapping name

-- | newvar generates a fresh type variable, not currently in use.
newvar :: TypeChecker Type
newvar = makename "a"

-- | makename keeps adding letters until a unique name is found.
makename :: Name -> TypeChecker Type
makename name = getUsed >>= \s -> case S.member name s of
  False -> return (TypeVar name)
  True -> let (c:cs) = reverse name in
    makename $ reverse (if c < 'z' then succ c:cs else 'a':c:cs)
  where getUsed = get >>= map fst ~> foldr collectVars S.empty ~> return
        collectVars :: TypeMap -> S.Set Name -> S.Set Name
        collectVars (TM mp) used = foldr getVars' used mp
        getVars' :: Type -> S.Set Name -> S.Set Name
        getVars' typ = S.union (getVars typ)

-- | getVars will find all of the type variables in a given type.
getVars :: Type -> S.Set Name
getVars (TypeVar name) = S.singleton name
getVars (a :=> b) = getVars a <> getVars b
getVars (NamedType _ ts) = mconcat $ map getVars ts

-- | unify is where restrictions get made
unify t1 t2 = case (t1, t2) of
  (TypeVar a, t) -> addRestriction a t
  (t, TypeVar a) -> addRestriction a t
  (a :=> b, c :=> d) -> unify a c >> unify b d
  (NamedType n ts, NamedType n' ts') | n == n' ->
    mapM_ (uncurry unify) (zip ts ts')
  (_, _) ->
    error $ "Types don't unify: " ++ render 0 t1 ++ " !: " ++ render 0 t2

refineType :: Type -> TypeChecker Type
refineType t = case t of
  TypeVar name -> do
    (Rs rs) <- getRestrictions
    prnt $ "Found these restrictions for " ++ name ++ ": " ++ show rs
    case M.lookup name rs of
      Nothing -> return t
      Just r -> refineType r
  t1 :=> t2 -> pure (:=>) <*> refineType t1 <*> refineType t2
  NamedType n ts -> NamedType n <$> mapM refineType ts

-- | getTypeOf searches our current scope for a variable and returns its
-- instantiated type.
getTypeOf name = do
  env <- get
  recursiveSearch name env
  where
    -- recursiveSearch searches recursively through our symbol table to find
    -- the type of the given symbol
    recursiveSearch name [] = error $ "Symbol `" ++ name ++ "` is not defined"
    recursiveSearch name ((TM mp, _):env) = case M.lookup name mp of
      Nothing -> recursiveSearch name env
      Just typ -> isUsed name >>= \case
        True -> return typ
        False -> instantiate typ
    -- instantiating just means making sure that any type variables
    -- this type introduces are unique. If we already have a variable `x`
    -- of type `a`, and `foo` is type `a -> b`, we don't want there to
    -- be any confusion with the types of `x` and `foo`.
    instantiate :: Type -> TypeChecker Type
    instantiate t = do
      -- find all the type variables vars and make a new one if they're used
      subs <- mapM newName (getVars t ! S.toList)
      return (apply (M.fromList subs) t)
      where
        -- newName will create a new name for it -- the same one if it's not
        -- already being used.
        newName :: Name -> TypeChecker (Name, Name)
        newName name = isUsed name >>= \case
          True -> newvar >>= \(TypeVar new) -> return (name, new)
          False -> return (name, name)
        -- apply looks all type variables up in a map and gives them
        -- their replacements
        apply :: M.Map Name Name -> Type -> Type
        apply mp t = case t of
          TypeVar v -> let Just v' = M.lookup v mp in TypeVar v'
          a :=> b   -> apply mp a :=> apply mp b
          NamedType n ts -> NamedType n (apply mp <$> ts)


initials = [(TM $ M.fromList
  [
    ("+", num :=> num :=> num)
    --("-", lit $ num :=> num :=> num),
    --("*", lit $ num :=> num :=> num),
    --("/", lit $ num :=> num :=> num),
    --("<", lit $ num :=> num :=> bool),
    --(">", lit $ num :=> num :=> bool),
    --("<=", lit $ num :=> num :=> bool),
    --(">=", lit $ num :=> num :=> bool),
    --("&&", lit $ bool :=> bool :=> bool),
    --("not", lit $ bool :=> bool),
    --("__matchFail__", witha a),
    --("__matchError__", witha a),
    --("[-]", witha $ a :=> a :=> a),
    --("Empty", witha $ n "List" [a]),
    --("::", witha $ a :=> n "List" [a] :=> n "List" [a]),
    --("undefined", witha a)
  ], Rs M.empty)]
  where a = TypeVar "a"
        n = NamedType

prnt :: String -> TypeChecker ()
prnt = lift . putStrLn
runInfer = infer ~> flip runStateT initials
test input = do
  (typ, env) <- input ! grab ! desugar ! runInfer
  putStrLn $ input ++ "\nis of type\n" ++ render 0 typ
  print env
