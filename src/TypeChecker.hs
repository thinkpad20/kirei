{-# LANGUAGE LambdaCase #-}
module TypeChecker (runInfer, typeCheck, typeCheckEnv, Env(..)) where

import Types
import AST
import Common
import Parser
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (foldl', foldr, lookup)

------------------------------------------------------------------------------
----- Inferrer machinery
------------------------------------------------------------------------------

-- | State of our type inferrer
type Inferrer = StateT Env IO

-- | tmap holds all determined types. @rs@ stands for restrictions and is a
-- temporary mapping from type variables to their refined types, generated
-- by @unify@.
data Env = Env {
    tmap::TypeMap,
    rs::M.Map Name Type,
    used::S.Set String
  } deriving (Show)

-- | It's useful to be able to pull all of the free variables out of our
-- environment.
instance FreeVars Env where
  free env =
    let freeFromTmap = free (tmap env) in
    let freeFromRestrs = unionAll (free <$> rs env) in
    freeFromTmap `S.union` freeFromRestrs `S.union` used env

-- | @defaultEnv@ stores some signatures of built-in types in @initials@.
defaultEnv = Env initials M.empty S.empty

-- | @runInferWith@ runs the inferrer on an expression with a specific
-- environment provided.
runInferWith :: Env -> Expr -> IO (Type, Env)
runInferWith env expr = runStateT (infer expr) env

-- | @runInfer@ runs the inferrer with the default environment.
runInfer :: Expr -> IO (Type, Env)
runInfer = runInferWith defaultEnv

test input = do
  expr <- grab input
  putStrLn $ "Desugared, this expression is " ++ show (desugar expr) ++ "\n\n"
  (typ, env) <- desugar expr ! runInfer
  putStrLn $ "Inferred\n   " ++ prettyExpr expr
  putStrLn $ "to be of type:\n   " ++ render 0 typ
  return env

test_ input = test input >> return ()

typeCheck :: String -> IO Type
typeCheck input = do
  expr <- desugar <$> grab input
  fst <$> runInfer expr

typeCheckEnv :: String -> IO Env
typeCheckEnv input = do
  expr <- desugar <$> grab input
  snd <$> runInfer expr

listT = TApply (TConst "[]")
maybeT = TApply (TConst "Maybe")

initials = TM $ M.fromList
  [
    ("__matchFail__", witha a),
    ("__matchError__", witha a),
    ("[-]", witha $ a :=> a :=> a),
    ("__listRange__", witha $ a :=> a :=> listT a)
  ]
  where witha = Polytype ["a"]
        a = TVar "a"
        b = TVar "b"

------------------------------------------------------------------------------
----- Type Inferrence algorithms
------------------------------------------------------------------------------

-- | @infer@ takes an expression and produces a type for that expression.
-- the @Inferrer@ state will also track all of the inferrences made during
-- the typing of the expression, for future use.
infer :: Expr -> Inferrer Type
infer expr = case expr of
  Number _ -> return num
  String _ -> return str
  Tuple es -> tuple <$> (mapM infer es >>= mapM refine)
  Var v -> get <!> tmap <!> tmLookup v >>= \case
    Nothing -> error $ "Variable `" ++ v ++ "` not defined in scope"
    Just t -> instantiate t
  TypeName n -> infer (Var n)
  Let name expr next -> do
    -- grab a copy of the current env
    env <- get
    -- infer the expression with name injected into the map
    (var, inferred) <- inject name expr
    -- unify the types and refine the new variable
    t <- unify var inferred >> refine var
    -- restore the env, generalize the type and store it
    put env >> generalize t >>= add name >> handle next
  Lambda pattern expr -> do
    -- infer the type of the pattern
    patternT <- inferPattern pattern
    env <- get
    bodyT <- infer expr
    refine (patternT :=> bodyT) -- <* put env
    where
      inferPattern pat = case pat of
        -- if it's a variable, give it a new type and add it to the env
        Var v -> newvar >>= \t -> add v (bare t) >> return t
        -- for a typename, we can just use the regular infer behavior
        TypeName n -> infer pat
        -- tuples are similar to normal expressions
        Tuple es -> tuple <$> (mapM inferPattern es >>= mapM refine)
        -- literals get returned as-is
        Number _ -> return num
        String _ -> return str
        -- application is almost the same but using inferPattern instead
        Apply a b -> do
          aT <- inferPattern a
          bT <- inferPattern b
          rT <- newvar
          unify aT (bT :=> rT)
          refine rT
        otherwise -> error $ "Illegal expression " ++ show pat ++ " in pattern"
  Apply func arg -> do
    -- infer the function type
    funcT <- infer func
    -- infer the argument type
    argT <- infer arg
    -- make a new variable for the result type
    resultT <- newvar
    -- funcT must be a function from argT to resultT
    unify funcT (argT :=> resultT)
    refine resultT
  If c t f -> do
    infer c >>= unify bool
    -- infer the true and false branches, unify them, and refine one of them
    infer t >>= \tT -> infer f >>= \fT -> unify tT fT >> refine tT
  -- TODO: sigs should not be able to be overwritten in the same scope
  Sig name typ next -> generalize typ >>= add name >> handle next
  _ -> error $ "Can't handle " ++ show expr ++ " (yet?)"
  where
    newvar = snd <$> newName "a"
    inject name expr = do
      new <- newvar
      inferred <- add name (bare new) >> infer expr
      return (new, inferred)
    add name polytype =
      modify (\env -> env { tmap = env ! tmap ! tmInsert name polytype })
    handle next = case next of
      -- if there's a next expression infer that
      Just next -> infer next
      -- else just return the unit
      Nothing   -> return $ tuple []

prnt :: String -> Inferrer ()
prnt = lift . putStrLn

-- | @instantiate@ creates a type from a polytype, by creating new type
-- variables (unused in the present scope) for all of the variables in
-- the polytype's list. Notice that if an @instantiate@ were to be immediately
-- followed by a @generalize@, the original polytype would be returned.
instantiate :: Polytype -> Inferrer Type
instantiate (Polytype vars t) = do
  subs <- mapM newName vars <!> M.fromList
  let newNameList = subs ! M.toList ! map snd ! map (\(TVar v) -> v)
  mapM_ addUsedName newNameList
  return $ apply subs t

-- | @generalize@ is the reverse of @instantiate@. It finds all of the
-- free variables in the type, removing all of the types which are free
-- in the current scope (since these are not bound by the type but instead
-- bound in the enclosing scope) and returns a Polytype with all of the
-- type's bound variables listed in the polytypes' variable list.
generalize :: Type -> Inferrer Polytype
generalize t = do
  let freeInType   = free t
  freeInScope     <- free <$> get
  let actuallyFree = freeInType S.\\ freeInScope
  pairs <- mapM newName (S.toList actuallyFree)
  let newNames = snd ~> (\(TVar v) -> v) <$> pairs
      subs = M.fromList pairs
  return $ Polytype newNames (apply subs t)

addUsedName :: Name -> Inferrer ()
addUsedName name = modify (\env -> env {used = S.insert name (env!used)})

-- | @newName@ takes a starting name and returns a tuple of that name paired
-- with a new type variable which is unused in the current scope.
newName name = new name "a" where
  -- see if name' is in the current free variables
  new :: Name -> Name -> Inferrer (Name, Type)
  new name name' = get <!> free <!> S.member name' >>= \case
    -- if the name is in the free variables, recurse with the next one
    True -> new name (next name')
    -- name is ok to use. Indicate we're using it and return the pairing
    False -> addUsedName name' >> return (name, (TVar name'))
  -- | next name takes the next letter in the alphabet unless we've gotten to
  -- z, in which case it adds on an a. Ex: a, b,... z, za, zb,... zz, zza, ..
  next :: Name -> Name
  next name = let (c:cs) = reverse name in
    reverse $ if c < 'z' then succ c : cs else 'a':c:cs

-- | @unify@ takes two types and generates whatever restrictions necessary
-- to ensure that they are equal. For example, unifying @a -> String -> b@
-- with @Number -> c@ will produce restrictions @{a: Number, c: String -> b}@.
-- Raises an error if two types cannot be made equal.
unify :: Type -> Type -> Inferrer ()
unify t1 t2 = do
  t1' <- refine t1
  t2' <- refine t2
  unify' t1' t2' where
    unify' a b = case (a, b) of
      -- if the types are the same don't do anything
      (a, b) | a == b -> return ()
      -- otherwise if one is a variable create a restriction
      (TVar v, t) -> restrict v t
      (t, TVar v) -> restrict v t
      -- otherwise recurse down
      (TApply a b, TApply c d) -> unify a c >> unify b d
      (TTuple ts, TTuple ts') -> zipWithM_ unify ts ts'
      (a :=> b, c :=> d) -> unify a c >> unify b d
      (a, b) -> error msg where
        msg = "Types can't unify: " ++ render 0 a ++ " !: " ++ render 0 b
    restrict v t = modify (\env -> env {rs = M.insert v t (env!rs)})

-- | @refine@ takes a type and traces it through the restrictions map,
-- applying all restrictions it finds, unless it detects a cycle in the
-- restrictions, for example a mapping from @a@ to @b -> a@, in which case
-- it raises an error.
refine :: Type -> Inferrer Type
refine t = r S.empty t where
  r :: S.Set Name -> Type -> Inferrer Type
  r seen t = case t of
    TConst name -> return $ TConst name
    TVar v -> case S.member v seen of
      True -> error "Cycle in types"
      False -> get <!> rs <!> M.lookup v >>= \case
        Nothing -> return t
        Just t' -> r (S.insert v seen) t'
    a :=> b -> pure (:=>) <*> r seen a <*> r seen b
    TApply a b -> pure TApply <*> r seen a <*> r seen b
    TTuple ts -> TTuple <$> mapM (r seen) ts
