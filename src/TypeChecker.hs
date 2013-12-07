{-# LANGUAGE LambdaCase #-}
import Types
import AST
import Common
import Parser
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Prelude hiding ((.), foldl', foldr, lookup)
import Debug.Trace

(.) = flip ($)
infixl 9 .

type Inferrer = StateT Env IO

data Env = Env {
    tmap::TypeMap,
    rs::M.Map Name Type,
    used::S.Set String
  } deriving (Show)


defaultEnv = Env initials M.empty S.empty

instance Types Env where
  free env =
    let freeFromTmap = free (tmap env) in
    let freeFromRestrs = unionAll (free <$> rs env) in
    freeFromTmap `S.union` freeFromRestrs `S.union` used env
  apply = undefined

infer :: Expr -> Inferrer Type
infer expr = case expr of
  Number _ -> return num
  String _ -> return str
  Var v -> lookup v >>= \case
    Nothing -> error $ "Variable `" ++ v ++ "` not defined in scope"
    Just t -> do
      prnt $ "found a mapping from " ++ v ++ " to " ++ render 0 t
      env <- get
      prnt $ "current env is " ++ show env
      t' <- instantiate t
      prnt $ "instantiated, " ++ render 0 t ++ " is " ++ render 0 t'
      return t'
  Let name expr next -> do
    prnt $ "Let 0) going to assign " ++ name ++ " to " ++ show expr
    env0 <- get
    prnt $ "Let 1) creating a newvar, env is " ++ show env0
    v <- newvar
    prnt $ "Let 2) newvar is " ++ show v ++ " so " ++ name ++ ": " ++ render 0 v
    inferred <- add name (bare v) >> infer expr
    env <- get
    prnt $ "Let 3) found expr " ++ show expr ++ " to be of type " ++ show inferred
    prnt $ "unifying " ++ show (v, inferred) ++ " under env " ++ show env
    unify (v, inferred)
    t <- refine v
    prnt $ "v refined is " ++ render 0 t
    put env0
    prnt $ "generalizing under env " ++ show env0
    t' <- generalize t
    prnt $ "generalized, that is " ++ render 0 t'
    add name t'
    case next of
      Just next -> infer next
      Nothing   -> return $ tuple []
  Lambda (Var name) expr -> do
    env0 <- get
    prnt $ "inferring a lambda " ++ show expr ++ ", env is " ++ show env0
    paramT <- newvar
    prnt $ "made a newvar for " ++ name ++ ", so " ++ name ++ ": " ++ render 0 paramT
    resultT <- add name (bare paramT) >> infer expr
    env <- get
    prnt $ "inferred the return of this lambda to be " ++ render 0 resultT ++ ", and env is " ++ show env
    t <- refine (paramT :=> resultT)
    prnt $ "so that means that this lambda is type " ++ render 0 t
    return t
  Apply func arg -> do
    env <- get
    prnt ("Apply 1) inferring func = " ++ show func ++ " with env " ++ show env)
    funcT <- infer func
    prnt ("Apply 2) funcT was " ++ render 0 funcT ++ ". Next inferring arg = " ++ show arg)
    argT <- infer arg
    prnt ("Apply 3) making a new var, argT was " ++ show argT)
    resultT <- newvar
    prnt ("Apply 3.5) the new var we made for resultT is " ++ show resultT)
    prnt ("Apply 4) unifying " ++ show (funcT, argT :=> resultT))
    unify (funcT, argT :=> resultT)
    refine resultT
  where add name scheme = modify (\env -> env { tmap = env ! tmap ! tmInsert name scheme })

prnt :: String -> Inferrer ()
prnt = \_ -> return ()
--prnt = putStrLn ~> lift
instantiate :: Scheme -> Inferrer Type
instantiate (Scheme vars t) = do
  subs <- M.fromList <$> mapM newName vars
  let newNameList = subs ! M.toList ! map (snd ~> (\(TypeVar v) -> v))
  when ((newNameList!length) > 0) $ prnt $ "These new names were generated: " ++ show newNameList
  mapM_ addUsedName newNameList
  env <- get
  when ((newNameList!length) > 0) $ prnt $ "After adding them, env is now " ++ show env
  return $ apply subs t

generalize :: Type -> Inferrer Scheme
generalize t = do
  let freeInType   = free t
  freeInScope     <- free <$> get
  let actuallyFree = freeInType S.\\ freeInScope
  pairs <- mapM newName (S.toList actuallyFree)
  let newNames = snd ~> (\(TypeVar v) -> v) <$> pairs
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
  new name name' = S.member name' <$> free <$> get >>= \case
    True -> new name (next name')
    False -> addUsedName name' >> return (name, (TypeVar name'))
  -- | next name takes the next letter in the alphabet unless we've gotten to
  -- z, in which case it adds on an a. Ex: a, b,... z, za, zb,... zz, zza, ..
  next :: Name -> Name
  next name = let (c:cs) = reverse name in
    reverse $ if c < 'z' then succ c : cs else 'a':c:cs

-- | newvar produces a new name, always starting with "a"
newvar :: Inferrer Type
newvar = snd <$> newName "a"

unify :: (Type, Type) -> Inferrer ()
unify (t1, t2) = do
  prnt $ "unifying " ++ show (t1, t2)
  t1' <- refine t1
  prnt $ render 0 t1 ++ " refined is " ++ render 0 t1'
  t2' <- refine t2
  prnt $ render 0 t2 ++ " refined is " ++ render 0 t2'
  u (t1', t2') where
    u (TypeVar v, TypeVar v') | v == v' = return ()
    u (TypeVar v, t) = prnt ("adding a restriction " ++ v ++ ": " ++ render 0 t) >> restrict v t
    u (t, TypeVar v) = prnt ("adding a restriction " ++ v ++ ": " ++ render 0 t) >> restrict v t
    u (a :=> b, c :=> d) = unify (a, c) >> unify (b, d)
    u (NamedType n ts, NamedType n' ts') | n == n' && length ts == length ts' =
      mapM_ unify $ zip ts ts'
    u (a, b) = do
      env <- get
      prnt $ "ruh roh, env is " ++ show env
      mismatchError a b
    restrict v t = modify (\env -> env {rs = M.insert v t (env!rs)})

mismatchError :: Type -> Type -> a
mismatchError a b = error $
  "Types can't unify: " ++ render 0 a ++ " !: " ++ render 0 b

refine :: Type -> Inferrer Type
refine t = r S.empty t where
  r :: S.Set Name -> Type -> Inferrer Type
  r seen t = case t of
    TypeVar v -> case S.member v seen of
      True -> error "Cycle in types"
      False -> M.lookup v <$> rs <$> get >>= \case
        Nothing -> return t
        Just t' -> r (S.insert v seen) t'
    a :=> b -> pure (:=>) <*> r seen a <*> r seen b
    NamedType name ts -> NamedType name <$> mapM (r seen) ts

lookup :: Name -> Inferrer (Maybe Scheme)
lookup name = tmLookup name <$> tmap <$> get

--test :: a -> (a -> Inferrer b) -> (b, Env)
test' f a = test f a defaultEnv
test f a env = do
  (b, env') <- runStateT (f a) env
  putStrLn $ render 0 b
  putStrLn $ render 0 (tmap env')
  print (rs env')

testInfer expr env = do
  (t, env') <- runStateT (infer expr) env
  putStrLn $ "Inferred\n   " ++ prettyExpr expr
  putStrLn $ "to be of type:\n   " ++ render 0 t
  putStrLn $ "All inferrences:\n" ++ render 0 (tmap env')
  when (length (M.toList $ rs env) > 0) $ print (rs env')

infer' :: String -> IO ()
infer' input = testInfer (desugar $ grab input) defaultEnv
