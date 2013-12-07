import Types hiding (free)
import AST
import Common
import Parser
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Prelude hiding ((.), foldl', foldr)
import Debug.Trace

(.) = flip ($)
infixl 9 .

data Env = Env {
    tmap::TypeMap,
    rs::TypeMap
  } deriving (Show)

infer :: Expr -> Env -> (Type, Env)
infer expr env = case expr of
  Number _ -> (num, env)
  String _ -> (str, env)
  Var v -> case tmLookup v (env!tmap) of
    Nothing -> error $ "Variable `" ++ v ++ "` not defined in scope"
    Just t -> instantiate t env
  Let name expr next ->
    let (v, env1) = env ! newvar
        (inferred, env2) = infer expr (env1 {tmap = tmInsert name (v!bare) (env1!tmap)})
        (refined, env3) = trace ("Let ) found expr " ++ show expr ++ " to be of type " ++ show inferred ++ ", refining with env " ++ show env) $ refine inferred env2
        finalEnv = env ! add name (generalize refined env3) in
    case next of
      Just next -> infer next finalEnv
      Nothing   -> (tuple [], finalEnv)
  Lambda (Var name) expr ->
    let (paramT, _) = env ! newvar
        (resultT, env1) = infer expr (add name (bare paramT) env) in
    refine (paramT :=> resultT) env1
  Apply func arg ->
    let (funcT, funcE) = trace ("Apply 1) inferring func = " ++ show func ++ " with env " ++ show env) $ infer func env
        (argT, argE) = trace ("Apply 2) inferring arg = " ++ show arg ++ " with env " ++ show funcE ++ ", funcT was " ++ show funcT) $ infer arg funcE
        (resultT, resultE) = trace ("Apply 3) making a new var, (argT, argE) was " ++ show (argT, argE)) $ (argE!newvar)
        unifiedE = trace ("Apply 4) unifying " ++ show (funcT, argT :=> resultT) ++ " with " ++ show resultE) $ unify resultE (funcT, argT :=> resultT) in
    trace ("Apply 5) unifiedE is " ++ show unifiedE ++ ", refined resultT is " ++ show (refine resultT unifiedE)) (resultT, unifiedE)
  where add name scheme env = env { tmap = env ! tmap ! tmInsert name scheme }
instantiate :: Scheme -> Env -> (Type, Env)
instantiate = undefined
generalize :: Type -> Env -> Scheme
generalize = undefined
newvar :: Env -> (Type, Env)
newvar = undefined
unify :: Env -> (Type, Type) -> Env
unify = undefined
refine :: Type -> Env -> (Type, Env)
refine = undefined


--instantiate :: Env -> Type -> (Type, Env)
--instantiate env t = (newT, env{names = (env.names) `S.union` newNames})
--  where
--    -- get the new type and the replacements made while making it
--    (newT, reps) = recurse tmEmpty t
--    -- make a set of used names from the replacements
--    newNames = used reps
--    recurse reps t = case t of
--      TypeVar name ->
--        case tmLookup name reps of
--          Just t -> (t, reps)
--          Nothing -> case S.member name used' of
--            False -> (t, tmInsert name t reps)
--            True -> (t', tmInsert name t' reps)
--              where (t', _) = recurse reps (TypeVar $ next name)
--        where used' = unionAll $ env.names : map used [env.bound, env.rs, reps]
--      a :=> b -> (aT :=> bT, reps'') where
--        (aT, reps') = recurse reps a
--        (bT, reps'') = recurse reps' b
--      NamedType n ts -> (NamedType n ts', reps') where
--        (ts', reps') = go ts reps
--        go [] reps = ([], reps)
--        go (t:ts) reps = (t' : ts', reps'') where
--          (t', reps') = recurse reps t
--          (ts', reps'') = go ts reps'

--used (TM m) = foldr S.union S.empty (find <$> m) where
--  find (TypeVar v) = S.singleton v
--  find (a :=> b) = find a `S.union` find b
--  find (NamedType _ ts) = foldr S.union S.empty (find <$> ts)
--next name = reverse (if c < 'z' then succ c : cs else 'a' : c : cs) where
--  (c : cs) = reverse name
--new name env = if S.member name (used env) then new (next name) env else name
--newvar :: Env -> Type
--newvar env = TypeVar $ new "a" where
--  used' = unionAll $ env.names : map used [env.bound, env.rs]
--  next name = let (c:cs) = reverse name in reverse $
--    if c < 'z' then succ c : cs else 'a' : c : cs
--  new name = if S.member name used' then new $ next name else name



--problem we have: we have unify (b->b, Number -> a)
--first we unify b with number, cool
--but then we unify b with a, not cool
--so we need to refine it first! yay
--buuut that's not good enough
--we should only unify when we see a var?

--unify (b->b, Number -> a), {}
--  refine (b -> b) -> (b -> b)
--  refine (Number -> a) -> (Number -> a)
--  u (b -> b, Number -> a)
--    unify

--infer (id id id)
--  -> (t, e) = infer (id id)
--    -> (argT, e) = infer id = (b->b, {}, {}, {b})
--      -> lookup id, free, instantiate (b -> b)
--    -> (funcT e) = infer id = (c->c, {}, {}, {b,c})
--      -> lookup id ({}, {}, {b}), free, initialize (c -> c)
--    -> resultT = newvar = d
--    -> unify (funcT, argT :=> resultT)
--      -> unify (c->c, (b->b) -> d)
--        -> refine c->c gives c->c
--        -> refine (b->b) -> d gives (b->b) -> d
--          u (c, b->b) ->




--unify :: Env -> (Type, Type) -> Env
--unify env (t1, t2) = u env (refine t1 env, refine t2 env) where
--  u env (TypeVar v, t) = env {rs = tmInsert v t (env.rs) }
--  u env (t, TypeVar v) = env {rs = tmInsert v t (env.rs) }
--  u env (a :=> b, c :=> d) = trace ("u function 3) env'' = " ++ show env'') env'' where
--    env'  = trace ("u function 1) unifying " ++ show (a, c) ++ " with " ++ show env) (unify env (a, c))
--    env'' = trace ("u function 2) unifying " ++ show (b, d) ++ " with " ++ show env') (unify env' (b, d))
--  u env (NamedType n ts, NamedType n' ts') | n == n' =
--    foldr envUnion envEmpty $ unify env <$> (zip ts ts')
--  u _ (t1, t2) = error $ "Types don't unify: " ++ show t1 ++ " !: " ++ show t2

--refine :: Type -> Env -> Type
--refine t env = r S.empty t where
--  r seen t = case t of
--    TypeVar v -> case S.member v seen of
--      True -> error "Cycle in types"
--      False -> case tmLookup v (env.rs) of
--        Nothing -> t
--        Just t' -> r (S.insert v seen) t'
--    a :=> b -> r seen a :=> r seen b
--    NamedType name ts -> NamedType name (r seen <$> ts)

--envEmpty = Env {bound=tmEmpty, free=tmEmpty, rs=tmEmpty, names=S.empty}
--test :: Expr -> (Type, Env)
--test expr = infer expr envEmpty

--envUnion env1 env2 = Env {
--    bound = env1.bound `tmUnion` env2.bound,
--    rs = env1.rs `tmUnion` env2.rs,
--    free  = env1.free `tmUnion` env2.free,
--    names = env1.names `nameUnion` env2.names
--  }

--runTest :: String -> IO ()
--runTest input = grab input ! desugar ! test ! p where
--  p (t, env) = putStrLn (render 0 t) >> print env
