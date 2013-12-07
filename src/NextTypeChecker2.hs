import Types hiding (free)
import AST
import Common
import Parser
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Prelude hiding ((.), lookup)
import Debug.Trace

(.) = flip ($)
infixl 9 .

data Env = Env {bound::TypeMap, rs::TypeMap, free::TypeMap} deriving (Show)

tmLookup name (TM m) = M.lookup name m
tmInsert name typ (TM m) = TM $ M.insert name typ m
tmUnion (TM m1) (TM m2) = TM $ M.union m1 m2
tmElems (TM m) = M.elems m
tmEmpty = TM M.empty
tmSingle name typ = TM $ M.singleton name typ

type UsedNames = S.Set Name
type TypeChecker = StateT UsedNames IO

type TypeChecker' = IO

infer :: Expr -> Env -> TypeChecker' (Type, Env)
infer expr env = case expr of
  Number _ -> return (num, env)
  String _ -> return (str, env)
  Var v -> case tmLookup v (env.bound) of
    Just t -> return (t, env)
    Nothing -> case tmLookup v (env.free) of
      Just t -> instantiate t env
      Nothing -> error $ "Variable `" ++ v ++ "` not defined in scope"
  Let name expr next -> do
    let env' = case tmLookup name (env.free) of
          Nothing -> env {bound = tmInsert name (env.newvar) (env.bound)}
          Just t -> env
    (t, env'') <- infer expr env'
    t' <- refine t (env''.rs)
    case next of
      Just next -> infer next (env{free = tmInsert name t' (env.free)})
      Nothing   -> return (tuple [], env)
  Lambda (Var name) expr -> do
    let env' = env { bound = tmInsert name (env.newvar) (env.bound) }
    (resultT, env'') <- infer expr env'
    paramT <- refine (tmLookup name (env'.bound) ! fromJust) (env''.rs)
    (paramT :=> resultT, env'')
  Apply func arg -> do
    (funcT, funcE) <- infer func env
    (argT, argE) <- infer arg (env { rs = tmUnion (env.rs) (funcE.rs) })
    v <- env.newvar
    rs3 <- unify (funcT, argT :=> v)
    return (refine v rs3, env {rs = tmUnion (env.rs) rs3})
  where
    instantiate :: Type -> Env -> TypeChecker' (Type, Env)
    instantiate t env = return (makeNew t env, env)
    makeNew :: Type -> Env -> Type
    makeNew t env = fst $ go t M.empty where
      go :: Type -> (M.Map Name Type) -> (Type, M.Map Name Type)
      go t reps = case t of
        TypeVar v -> if not (S.member v (used env)) then (t, reps) else case M.lookup v reps of
          Just t -> (t, reps)
          Nothing -> let v' = TypeVar $ new v env in (v', M.insert v  v' reps)
        a :=> b   ->
          let (aT, reps') = go a reps
              (bT, reps'') = go b reps' in
          (aT :=> bT, reps'')
        NamedType n ts ->
          let go' reps [] = ([], reps)
              go' reps (t:ts) = (t' : ts, M.union reps others) where
                (t', reps') = go t reps
                (ts, others) = go' reps' ts
          (NamedType n (fst $ go' reps ts), snd $ go' reps ts)
    getVars :: Type -> S.Set Name
    getVars (TypeVar v) = S.singleton v
    getVars (a :=> b) = getVars a `S.union` getVars b
    getVars (NamedType _ ts) = foldr S.union S.empty (getVars <$> ts)
    allVars env = getVars <$> concatMap tmElems [env.bound, env.free, env.rs]
    used env = foldr S.union S.empty (allVars env)
    next name = let (c : cs) = reverse name in
      reverse $ if c < 'z' then succ c : cs else 'a' : c : cs
    new name env = if S.member name (used env) then new (next name) env else name
    newvar :: Env -> TypeChecker' Type
    newvar env = return $ TypeVar $ new "a" where
      next name = let (c:cs) = reverse name in reverse $
        if c < 'z' then succ c : cs else 'a' : c : cs
      new name = if S.member name (used env) then new $ next name else name

unify :: (Type, Type) -> TypeChecker' TypeMap
unify (TypeVar v, t) = return $ tmSingle v t
unify (t, TypeVar v) = return $ tmSingle v t
unify (a :=> b, c :=> d) = pure tmUnion <*> unify (a, c) <*> unify (b,d)
unify (NamedType n ts, NamedType n' ts') | n == n' =
  tms <- mapM unify (zip ts ts')
  return $ foldr tmUnion tmEmpty tms
unify (t1, t2) = error $ "Types don't unify: " ++ show t1 ++ " !: " ++ show t2

refine :: Type -> TypeMap -> TypeChecker' Type
refine t rs = return $ r S.empty t where
  r seen t = case t of
    TypeVar v -> case S.member v seen of
      True -> error "Cycle in types"
      False -> case tmLookup v rs of
        Nothing -> t
        Just t' -> r (S.insert v seen) t'
    a :=> b -> r seen a :=> r seen b
    NamedType name ts -> NamedType name (r seen <$> ts)

envEmpty = Env {bound=tmEmpty, free=tmEmpty, rs=tmEmpty}
test :: Expr -> TypeChecker' (Type, Env)
test expr = infer expr envEmpty

runTest :: String -> IO ()
runTest input = grab input ! desugar ! test >>= p where
  p (t, env) = putStrLn (render 0 t) >> print env
