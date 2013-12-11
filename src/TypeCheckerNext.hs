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
    used::S.Set String,
    namespace::[Name]
  } deriving (Show)

-- | It's useful to be able to pull all of the free variables out of our
-- environment.
instance FreeVars Env where
  free env = free (tmap env) `S.union` used env

-- | @defaultEnv@ stores some signatures of built-in types in @initials@.
defaultEnv = Env initials M.empty S.empty ["(root)"]

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

initials = TM $ M.fromList
  [
    ("(root).[]", witha $ listT a),
    ("(root).::", witha $ a :=> listT a :=> listT a),
    ("(root).__matchFail__", witha a),
    ("(root).__matchError__", witha a),
    ("(root).[-]", witha $ a :=> a :=> a),
    ("(root).__listRange__", witha $ a :=> a :=> listT a)
  ]
  where witha = Polytype ["a"]
        a = TVar "a"
        b = TVar "b"
        listT = TApply (TConst "[]")
        maybeT = TApply (TConst "Maybe")

pushName :: Name -> Inferrer ()
pushName name = modify $ (\env -> env {namespace = name : namespace env})

popName :: Inferrer Name
popName = do
  name <- get <!> namespace <!> head
  modify $ (\env -> env {namespace = tail $ namespace env})
  return name

getFullName :: Name -> [Name] -> Name
getFullName name nspace = intercalate "." $ reverse $ name : nspace

-- | @lookup@ looks in the current namespace for a variable, as opposed to
-- deepLookup which will go backwards.
lookup :: Name -> Inferrer (Maybe Polytype)
lookup name = do
  fullname <- get <!> namespace <!> getFullName name
  get <!> tmap <!> tmLookup fullname

-- | @deepLookup@ recurses backwards through the namespace, trying all
-- possible names until it finds a matching scheme (or returns @Nothing@)
deepLookup :: Name -> Inferrer (Maybe Polytype)
deepLookup name = do
  nspace <- namespace <$> get
  recurse nspace
  where recurse [] = return Nothing
        recurse nspace =
          get <!> tmap <!> tmLookup (getFullName name nspace) >>= \case
            Nothing -> recurse (tail nspace)
            Just scheme -> return $ Just scheme

------------------------------------------------------------------------------
----- Type Inferrence algorithms
------------------------------------------------------------------------------

monad >>== function = monad >>= \a -> function a >> return a
infixl 1 >>==

-- | @infer@ takes an expression and produces a type for that expression.
-- the @Inferrer@ state will also track all of the inferrences made during
-- the typing of the expression, for future use.
infer :: Expr -> Inferrer Type
infer expr = case expr of
  Number _ -> return num
  String _ -> return str
  Tuple es -> tuple <$> (mapM infer es >>= mapM refine)
  Var v -> deepLookup v >>= \case
    Nothing -> error $ "Variable `" ++ v ++ "` not defined in scope"
    Just t -> instantiate t
  TypeName n -> infer (Var n)
  Let name expr next -> do
    t <- lookup name >>= \case
      -- if the type has been declared already, we can return it
      Just someType -> instantiate someType
      -- if it doesn't exist, make a new one and add it to the env
      Nothing -> newvar >>== add name . bare
    -- push a new namespace, infer the expression and unify it, then pop
    pushName name >> infer expr >>= unify t >> popName
    -- refine the type, generalize it, register it in the scope and move on
    refine t >>= generalize >>= register name >> handle next
  Lambda pattern expr -> do
    -- infer the type of the pattern
    patternT <- inferPattern pattern
    bodyT    <- infer expr
    -- removes all of the variables in the pattern
    cleanup pattern
    refine (patternT :=> bodyT)
    where
      inferPattern pat = case pat of
        -- if it's a variable, give it a new type and add it to the env
        Var v -> lookup v >>= \case
          Nothing -> newvar >>= \t -> add v (bare t) >> return t
          Just _ -> error $ "`" ++ v ++ "` is already defined in scope"
        -- for a typename, we can just use the regular infer behavior
        TypeName n -> infer pat
        -- tuples are almost the same as normal expressions
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
      cleanup pattern = case pattern of
        Var v -> remove v
        Apply a b -> cleanup a >> cleanup b
        Tuple es -> mapM_ cleanup es
        _ -> return ()
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
    add name polytype = do
      fullname <- get <!> namespace <!> getFullName name
      modify (\env -> env { tmap = env ! tmap ! tmInsert fullname polytype })
    handle next = case next of
      -- if there's a next expression infer that
      Just next -> infer next
      -- else just return the empty tuple type
      Nothing   -> return $ tuple []
    register :: Name -> Polytype -> Inferrer ()
    register = add
    remove name = do
      fullname <- get <!> namespace <!> getFullName name
      lookup name >>= \case
        Just (Polytype [] (TVar tname)) -> removeUsedName tname
        Nothing -> error $ "Weird thing when removing " ++ fullname
      modify (\env -> env {tmap = env ! tmap ! tmDelete fullname})

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

removeUsedName :: Name -> Inferrer ()
removeUsedName name = do
  modify (\env -> env {used = S.delete name (env!used)})

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
-- it raises an error. It has the side effect of updating the environment
-- with a new set of restrictions which don't involve the type.
-- For example, if our restrictions table currently has
-- @{a => b->c, d => N -> a, c => b}@, then after @updateRs@ it would be
-- @{d => N -> b -> b}@.
refine :: Type -> Inferrer Type
refine (TVar name) = do
  prnt $ "refining " ++ show name
  get <!> rs <!> M.lookup name >>= \case
    Nothing -> return $ TVar name
    Just t -> do
      modify $ (\env -> env { rs = M.delete name (rs env)})
      update name t >> return t
  where
    update name type_ = do
      prnt $ "updating types containing " ++ show name
      modify $ (\env -> env { rs = fmap update' (rs env)}) where
        -- ok so we're looking to update all of the type have a type,
        -- if it's constant we don't need to do anything
        update' origType = case origType of
          TConst n -> TConst n
        -- if it's a variable, we need to make sure we haven't already seen it
          TVar n | name == n -> type_
                 | otherwise -> TVar n
          TApply a b -> update' a `TApply` update' b
          TTuple ts -> TTuple $ map update' ts
          t1 :=> t2 -> update' t1 :=> update' t2
refine type_ = return type_
