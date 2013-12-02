module AlgorithmW  (  Expr(..),
                      Type(..),
                      infer
                   ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (lookup)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Text.PrettyPrint as PP
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Parser

data Type =
  TVar Name
  | NumberType
  | StringType
  | TupleType [Type]
  | NamedType Name [Type]
  | Type :=> Type
  deriving (Eq, Ord)

data Polytype = Polytype [Name] Type

instance Show Polytype where
  show (Polytype vars t) = loop vars where
    loop [] = show t
    loop (v:vs) = "∀" ++ v ++ "." ++ loop vs

class Types a where
  free :: a -> S.Set Name
  applySub :: Substitutions -> a -> a

(∪) = S.union
(∅) = S.empty

instance Types Type where
  free (TVar n) = S.singleton n
  free NumberType = (∅)
  free StringType = (∅)
  free (t1 :=> t2) = free t1 ∪ free t2

  applySub s (TVar n) = case M.lookup n s of
    Nothing  -> TVar n
    Just t   -> t
  applySub s (t1 :=> t2) = applySub s t1 :=> applySub s t2
  applySub s t = t

instance Types Polytype where
  free (Polytype vars t) =
    (free t) `S.difference` (S.fromList vars)
  applySub s (Polytype vars t) =
    Polytype vars (applySub (foldr M.delete s vars) t)

instance Types a => Types [a] where
  applySub s = map (applySub s)
  free l = foldr (∪) (∅) (free <$> l)

type Substitutions = M.Map String Type

noSubstitutions :: Substitutions
noSubstitutions = M.empty

composeSubs :: Substitutions -> Substitutions -> Substitutions
composeSubs s1 s2 = (applySub s1 <$> s2) `M.union` s1

newtype TypeEnv = TypeEnv (M.Map String Polytype)

instance Show TypeEnv where
  show (TypeEnv env) = let
    pairs = M.toList env
    toS (key, val) = show key ++ " => " ++ show val
    in "{" ++ (intercalate ", " $ map toS pairs) ++ "}"

remove :: Name -> TypeEnv -> TypeEnv
remove var (TypeEnv env) = TypeEnv (M.delete var env)

addMapping :: TypeEnv -> TypeEnv -> TypeEnv
addMapping (TypeEnv te1) (TypeEnv te2) = TypeEnv $ te1 `M.union` te2

(==>) :: Name -> Polytype -> TypeEnv
n ==> s = TypeEnv $ M.singleton n s

instance Types TypeEnv where
  free (TypeEnv env) = free (M.elems env)
  applySub s (TypeEnv env) = TypeEnv (applySub s <$> env)

generalize :: Type -> Inferrer Polytype
generalize t = do
  env <- namesToPolytypes <$> get
  let vars = S.toList (free t `S.difference` free env)
  return $ Polytype vars t

data InferrerState =
  InferrerState {
    inferSupply :: Int,
    inferSubstitutions :: Substitutions,
    namesToTypes :: M.Map Name Type,
    namesToPolytypes :: TypeEnv
  }

type Inferrer = ErrorT String (StateT InferrerState IO)

runInferrer :: Inferrer a -> IO (Either String a, InferrerState)
runInferrer t = runStateT (runErrorT t) initial
  where initial = InferrerState {
                    inferSupply = 0,
                    inferSubstitutions = noSubstitutions,
                    namesToTypes = M.empty,
                    namesToPolytypes = TypeEnv M.empty
                  }

-- | Get a fresh new type variable to use starting with the given prefix
newTVar :: String -> Inferrer Type
newTVar prefix = do
  -- get the current state
  s <- get
  -- increment the inferSupply
  put s { inferSupply = inferSupply s + 1 }
  -- wrap it in a type variable and return it
  let var = TVar $ prefix ++ show (inferSupply s)
  prnt $ "Got a new tvar: " ++ show var
  return var

instantiate :: Polytype -> Inferrer Type
instantiate p@(Polytype vars t) = do
  prnt $ "instantiate, input is " ++ show p
  newVars <- mapM (\_ -> newTVar "a") vars
  -- make a substitution mapping all of the variables in the scheme
  -- to new variables we just generated
  prnt $ "Made new variables: " ++ show newVars
  let s = M.fromList (zip vars newVars)
  prnt $ "Made substitutions: " ++ show s
  return $ applySub s t

unify :: Type -> Type -> Inferrer Substitutions
a `unify` b = case (a,b) of
  (l :=> r, l' :=> r') -> do
    s1 <- l `unify` l'
    s2 <- applySub s1 r `unify` applySub s1 r'
    return (s1 `composeSubs` s2)
  (TVar u, t) -> u `link` t
  (t, TVar u) -> u `link` t
  (NumberType, NumberType) -> return noSubstitutions
  (StringType, StringType) -> return noSubstitutions
  (t1, t2) -> throwError $
    "Type '" ++ show t1 ++ "' cannot be unified with '" ++ show t2 ++ "'"
  where
    link :: Name -> Type -> Inferrer Substitutions
    link name typ | typ == TVar name = return noSubstitutions
                  | name `S.member` free typ = do
                      prnt $ "Ah crap. t is " ++ show typ
                      prnt $ "free t is " ++ show (free typ)
                      throwError $ "occur check fails: " ++ name ++ ", " ++ show typ
                  | otherwise = return (M.singleton name typ)

prnt = lift . lift . putStrLn

updateEnv :: (TypeEnv -> TypeEnv) -> Inferrer TypeEnv
updateEnv f = do
  state <- get
  let newEnv = f $ namesToPolytypes state
  put state {namesToPolytypes = newEnv}
  return newEnv

teLookup :: Name -> TypeEnv -> Maybe Polytype
teLookup name (TypeEnv env) = M.lookup name env



infer :: Expr -> Inferrer (Substitutions, Type)
infer expr = case expr of
  -- symbols are same as variables in this context
  Symbol s -> infer $ Var s
  -- If we encounter a variable, we need to look it up
  Var name -> do
    polys <- namesToPolytypes <$> get
    case teLookup name polys of
      Nothing -> throwError $ "unbound variable: " ++ name
      Just σ -> do
        -- we need to instantiate a new type variable
        t <- instantiate σ
        return (noSubstitutions, t)
  String _ -> return (noSubstitutions, StringType)
  Number _ -> return (noSubstitutions, NumberType)
  Lambda varName e -> do
    newT <- newTVar "a"
    -- remove this variable name from the polytype mapping
    updateEnv $ remove varName
    -- add mapping {varName => (Polytype [] newT)} to our current env
    updateEnv $ addMapping (varName ==> Polytype [] newT)
    (s1, t1) <- infer e
    updateEnv $ remove varName
    prnt $ "finished lambda, returning " ++ show (s1, applySub s1 newT :=> t1)
    return (s1, applySub s1 newT :=> t1)
  Apply func arg -> do
    newT <- newTVar "a"
    (argS, argT) <- infer arg
    prnt $ "IN APPLY inferred " ++ show (argS, argT) ++ " from " ++ show arg
    updateEnv (applySub argS)
    (funcS, funcT) <- infer func
    prnt $ "IN APPLY inferred " ++ show (funcS, funcT) ++ " from " ++ show func
    prnt "gonna try to unify"
    prnt$ "means I'm gonna apply subs: " ++ show funcS
    prnt$ "to the type: " ++ show argT
    prnt $ "free (t2 -> tv) = " ++ show (free (funcT :=> newT))
    resS <- unify (applySub funcS argT) (funcT :=> newT)
    return (resS `composeSubs` funcS `composeSubs` argS,
            applySub resS newT)
  Let x body next -> do
    (bodySubs, bodyT) <- infer body
    prnt $ "Inferring " ++ show body ++ " gave us " ++ show (bodySubs, bodyT)
    env <- getEnv
    prnt $ "About to remove " ++ show x ++ " from " ++ show env
    updateEnv (remove x)
    env' <- getEnv
    prnt $ "Env after removing x: " ++ show env'
    updateEnv (applySub bodySubs)
    env <- getEnv
    prnt $ "Env after applying substitutions: " ++ show env
    t' <- generalize bodyT
    updateEnv (\(TypeEnv env) -> TypeEnv $ M.insert x t' env)
    case next of
      Nothing -> return (bodySubs, TupleType [])
      Just expr -> do
        (nextSubs, nextType) <- infer expr
        return (bodySubs `composeSubs` nextSubs, nextType)

getEnv = namesToPolytypes <$> get

typeInference :: Expr -> Inferrer Type
typeInference e = uncurry applySub <$> infer e

{- TESTING... -}

e0 = grab "let id = \\x -> x; id;"
e1 = grab "let id = \\x -> x; id id;"
e2 = grab "let id = \\x -> let y = x; y; id id;"
e3 = grab "let id = \\x -> let y = x; y; id id 2;"
-- λx . x x should fail
e4 = grab "let id = \\x -> x x; id;"
e5 = grab "\\m -> let y = m; let x = y \"hello\"; x;"

testInfer :: (Int, Expr) -> IO ()
testInfer (i, e) = do
  putStrLn $ "Test " ++ show i ++ ":\n\t'" ++ show e ++ "'\n"
  (res, _) <- runInferrer $ typeInference e
  case res of
    Left err -> putStrLn $ "error: " ++ err ++ "\n\n"
    Right t  -> putStrLn $ prettyExpr e ++ " is of type " ++ show t ++ "\n\n"

main :: IO ()
main = mapM_ testInfer $ zip [0..] [e0, e1, e2, e3, e4, e5]


instance Show Type where
  show t = case t of
    NumberType -> "Number"
    StringType -> "String"
    TVar name -> name
    TupleType ts -> concat ["(", intercalate ", " $ show <$> ts, ")"]
    NamedType name ts -> name ++ case ts of
      [] -> ""
      [t] -> " " ++ show' t
      ts -> "(" ++ intercalate " " (show' <$> ts) ++ ")"
    t1 :=> t2 -> case t1 of
      _ :=> _ -> concat ["(", show t1, ")", " -> ", show t2]
      _ -> show t1 ++ " -> " ++ show t2
    where show' NumberType = show NumberType
          show' StringType = show StringType
          show' (TVar n) = n
          show' (NamedType n []) = show n
          show' t@(TupleType _) = show t
          show' t = "(" ++ show t ++ ")"
