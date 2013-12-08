{-# LANGUAGE LambdaCase #-}
import Types
import AST
import Common
import Parser
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (foldl', foldr, lookup)

type Inferrer = StateT Env IO

data Env = Env {
    tmap::TypeMap,
    rs::M.Map Name Type,
    used::S.Set String
  } deriving (Show)


defaultEnv = Env initials M.empty S.empty

instance FreeVars Env where
  free env =
    let freeFromTmap = free (tmap env) in
    let freeFromRestrs = unionAll (free <$> rs env) in
    freeFromTmap `S.union` freeFromRestrs `S.union` used env

infer :: Expr -> Inferrer Type
infer expr = case expr of
  Number _ -> return num
  String _ -> return str
  Bool   _ -> return bool
  Tuple es -> tuple <$> (mapM infer es >>= mapM refine)
  Var v -> get <!> tmap <!> tmLookup v >>= \case
    Nothing -> error $ "Variable `" ++ v ++ "` not defined in scope"
    Just t -> instantiate t
  Let name expr next -> do
    -- grab a copy of the current env
    env <- get
    -- infer the expression with name injected into the map
    (var, inferred) <- inject name expr
    -- unify the types and refine the new variable
    t <- unify var inferred >> refine var
    -- restore the env, generalize the type and store it
    put env >> generalize t >>= add name
    case next of
      -- if there's a next expression infer that
      Just next -> infer next
      -- else just return the unit
      Nothing   -> return $ tuple []
  Lambda (Var name) expr -> do
    -- inject the variable name in and infer the expression
    (paramT, resultT) <- inject name expr
    -- a lambda must be a function from paramT to resultT
    refine (paramT :=> resultT)
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
    infer t >>= \tT -> infer f >>= \fT -> unify tT fT >> refine tT
  _ -> error $ "Can't handle " ++ show expr ++ " yet"
  where
    newvar = snd <$> newName "a"
    inject name expr = do
      new <- newvar
      inferred <- add name (bare new) >> infer expr
      return (new, inferred)
    add name scheme = modify (\env -> env { tmap = env ! tmap ! tmInsert name scheme })

instantiate :: Scheme -> Inferrer Type
instantiate (Scheme vars t) = do
  subs <- mapM newName vars <!> M.fromList
  let newNameList = subs ! M.toList ! map snd ! map (\(TVar v) -> v)
  mapM_ addUsedName newNameList
  return $ apply subs t

generalize :: Type -> Inferrer Scheme
generalize t = do
  let freeInType   = free t
  freeInScope     <- free <$> get
  let actuallyFree = freeInType S.\\ freeInScope
  pairs <- mapM newName (S.toList actuallyFree)
  let newNames = snd ~> (\(TVar v) -> v) <$> pairs
      subs = M.fromList pairs
  return $ Scheme newNames (apply subs t)

-- | setStartName resets the name to `s`
addUsedName :: Name -> Inferrer ()
addUsedName name = modify (\env -> env {used = S.insert name (env!used)})

-- | newName takes a starting name and returns a tuple of that name paired
-- with a new type variable which is unused in the current scope.
newName name = new name "a" where
  -- see if name' is in the current free variables
  new :: Name -> Name -> Inferrer (Name, Type)
  new name name' = get <!> free <!> S.member name' >>= \case
    True -> new name (next name')
    False -> addUsedName name' >> return (name, (TVar name'))
  -- | next name takes the next letter in the alphabet unless we've gotten to
  -- z, in which case it adds on an a. Ex: a, b,... z, za, zb,... zz, zza, ..
  next :: Name -> Name
  next name = let (c:cs) = reverse name in
    reverse $ if c < 'z' then succ c : cs else 'a':c:cs

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

runInfer expr env = do
  (t, env') <- runStateT (infer expr) env
  putStrLn $ "Inferred\n   " ++ prettyExpr expr
  putStrLn $ "to be of type:\n   " ++ render 0 t
  --putStrLn $ "All inferrences:\n" ++ render 0 (tmap env')
  when (length (M.toList $ rs env) > 0) $ print (rs env')

infer' :: String -> IO ()
infer' input = runInfer (desugar $ grab input) defaultEnv
