{-# LANGUAGE LambdaCase #-}
module TypeChecker where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import AST
import Parser
import Types
import Common

type UsedNames = S.Set Name
type Env = ([TypeEnvironment], UsedNames)

type TypeChecker = StateT Env IO

num  = NamedType "Number" []
str  = NamedType "String" []
bool = NamedType "Bool" []
tuple = NamedType ""

getUsed :: TypeChecker UsedNames
getUsed = get >>= \(_, used) -> return used

addUsed :: Name -> TypeChecker ()
addUsed name = modify (\(env, used) -> (env, S.insert name used))

freeUsed :: Name -> TypeChecker ()
freeUsed name = modify (\(env, used) -> (env, S.delete name used))

generalize :: TypeEnvironment -> Type -> Scheme
generalize env t = Scheme (S.toList $ free t S.\\ free env) t

instantiate :: Scheme -> TypeChecker Type
instantiate (Scheme vars term) = do
  newVars <- mapM makename vars
  return $ apply (M.fromList  $ zip vars newVars) term

newvar :: TypeChecker Type
newvar = makename "a"

makename :: Name -> TypeChecker Type
makename name = getUsed >>= \s -> case S.member name s of
  False -> addUsed name >> pure (TypeVar name)
  True -> let (c:cs) = reverse name in
    makename $ reverse (if c < 'z' then succ c:cs else 'a':c:cs)

infer :: Expr -> TypeChecker Type
infer expr = case expr of
  Number _ -> return num
  String _ -> return str
  Bool   _ -> return bool
  Var    x -> getTypeOf x
  Tuple exprs -> do
    tuple <$> mapM infer exprs
  Lambda (Var x) e  -> do
    -- need to be able to read the environment... this is tough
    -- could have some kind of queue like:
    -- sig foo : a -> b -> c pushes a, b, c.
    -- let foo = \x -> y we see a lambda so we consume a, we know
    -- x is a. But what would be better is to have this done when
    -- we do a let, not a sig. So for example when we say let foo =,
    -- we look up foo and push a, then b, then c. Then we read that
    -- stack when we do the lambda. Of course that would mean all lambdas
    -- must be assigned variables, which blows. Another strategy?
    {-
      \f -> map f [1..10]
      We withhold judgment on the variable until we see how it's used. We know
      map is (a -> b) -> [a] -> [b], so we know f : (a -> b), and we know
      [1..10] : [Number], so a is a Number, and f : (Number -> b). Then
      \f -> map f [1..10] : (Number -> b) -> [b]
      We can tell this by a series of specializations.
      What that means then is we start by saying f : newvar (let's say "x")
      Then we apply map to f so we get (a -> b) -> ... from that inferrence,
      then we unify (a -> b) with x, so f : (a -> b) (of course it still has a
      and b in its scheme). In the next step we apply (map f) to [1..10] and
      (map f) has the type [a] -> [b], so we unify [a] with [Number] which means
      a is Number, which means all `a`s in the current scope can be collapsed to
      Number, so f : Number -> b (only b in its scheme now), and we return a [b].

      f : \/x.x
      map : \/a b. (a->b) -> [a] -> [b]
      [1,2,3] -> [Number]
      apply map f [1,2,3]
        -> infer (map f)
          -> apply map f
            -> infer (map)
              -> lookup, find \/a b. (a->b) -> [a] -> [b]
                -> instantiate, get (a' -> b') -> [a'] -> [b']
                -> return (a' -> b') -> [a'] -> [b']
            map is a -> b where a : (a' -> b') and b : ([a'] -> [b'])
            -> infer (f)
              -> lookup, find \/x. x
                -> instantiate, get x'
                -> return x'
            unify (a' -> b') with x'
              -> x' is a typevar, so update environment, replacing x's with (a' -> b')
              -> problem: there is no x' in our environment, only x.x
    -}
    t@(TypeVar name) <- newvar
    pushEnvWith x (Scheme [name] t)
    t' <- infer e
    argType <- getTypeOf x
    popEnv
    return $ argType :=> t'
  Sig name typ e -> do
    addToEnv name (Scheme [] typ)
    case e of
      Nothing -> return $ tuple []
      Just e -> infer e
  Apply e1 e2 -> do
    t1 <- infer e1
    prnt $ "inferred t1 to be " ++ show t1
    case t1 of
      a :=> b -> do
        t2 <- infer e2
        prnt $ "inferred t2 to be " ++ show t2
        unify a t2
        prnt $ "type of " ++ show expr ++ " to be " ++ show b
        return b
      otherwise -> error $ "Expression `" ++ show e1 ++ "` is not a function"
  Let var e1 e2 -> do
    recorded <- getTypeOf var
    t1 <- infer e1
    unify recorded t1
    case e2 of
      Nothing -> return $ tuple []
      Just e2 -> infer e2
  If c t f -> do
    cType <- infer c
    cType `unify` bool
    tType <- infer t
    fType <- infer f
    tType `unify` fType
    return tType
  _ -> error $ "What's this? " ++ show expr
  where pushEnvWith name typ = pushEnv >> addToEnv name typ
        addToEnv name typ = do
          ((TE env):envs, used) <- get
          put ((TE $ M.insert name typ env):envs, used)
        pushEnv = get >>= \(envs, used) -> put (TE M.empty:envs, used)
        unify t1 t2 = case (t1, t2) of
          (TypeVar a, t) -> substitute a t
          (t, TypeVar a) -> substitute a t
          (a :=> b, c :=> d) -> unify a c >> unify b d
          (NamedType n ts, NamedType n' ts') | n == n' ->
            mapM_ (uncurry unify) (zip ts ts')
          (_,_) -> error $ "Types don't unify: " ++ r t1 ++ " !: " ++ r t2
          where r = render 0
        popEnv = get >>= \(_:envs, used) -> put (envs, used)
        getEnvs = get >>= \(envs, _) -> return envs
        getTypeOf name = getTypeOf' name >>= instantiate
        getTypeOf' name = do
          envs <- getEnvs
          return $ lookup envs where
            lookup [] = error $ "Type of " ++ name ++ " is unknown"
            lookup (TE env:envs) =
              case M.lookup name env of
                Just typ -> typ
                Nothing -> lookup envs
        substitute name typ = do
          (env:envs, used) <- get
          put (apply (M.singleton name typ) <$> env: envs, used)


initials = [TE $ M.fromList
  [
    ("+", lit $ num :=> num :=> num)
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
  ]]
  where lit = Scheme []
        witha = Scheme ["a"]
        a = TypeVar "a"
        cons = Scheme ["a"]
        n = NamedType

prnt :: String -> TypeChecker ()
prnt = lift . putStrLn
runInfer = infer ~> flip runStateT (initials, S.singleton "a")
test input = do
  (typ, env) <- input ! grab ! desugar ! runInfer
  putStrLn $ input ++ "\nis of type\n" ++ render 0 typ
  putStrLn $ render 0 typ ++ ", " ++ render 0 env
