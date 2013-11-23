module AlgorithmW  (  Expr(..),
                      Type(..),
                      infer
                   ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Text.PrettyPrint as PP
import Control.Applicative ((<$>))
import Parser

data Type =
  TVar String
  | NumberType
  | StringType
  | TupleType [Type]
  | NamedType String [Type]
  | Type :=> Type
  deriving (Eq, Ord)

data Scheme = Scheme [String] Type

class Types a where
  free :: a -> S.Set String
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

instance Types Scheme where
  free (Scheme vars t) =
    (free t) `S.difference` (S.fromList vars)
  applySub s (Scheme vars t) =
    Scheme vars (applySub (foldr M.delete s vars) t)

instance Types a => Types [a] where
  applySub s = map (applySub s)
  free l = foldr (∪) (∅) (map free l)

type Substitutions = M.Map String Type

noSubstitutions :: Substitutions
noSubstitutions = M.empty

composeSubs :: Substitutions -> Substitutions -> Substitutions
composeSubs s1 s2 = (applySub s1 <$> s2) `M.union` s1

newtype TypeEnv = TypeEnv (M.Map String Scheme)

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

instance Types TypeEnv where
  free (TypeEnv env) = free (M.elems env)
  applySub s (TypeEnv env) = TypeEnv (applySub s <$> env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = S.toList (free t `S.difference` free env)


data InferrerEnv = InferrerEnv (M.Map String Type)

data InferrerState =
  InferrerState {
    inferSupply :: Int,
    inferSubstitutions :: Substitutions,
    namesToTypes :: M.Map Name Type,
    namesToSchemes :: TypeEnv
  }



type Inferrer a =
  ErrorT String (ReaderT InferrerEnv (StateT InferrerState IO)) a

runInferrer :: Inferrer a -> IO (Either String a, InferrerState)
runInferrer t =
    runStateT (runReaderT (runErrorT t) initInferrerEnv) initInferrerState
  where initInferrerEnv = InferrerEnv M.empty
        initInferrerState = InferrerState {
                              inferSupply = 0,
                              inferSubstitutions = noSubstitutions,
                              namesToTypes = M.empty,
                              namesToSchemes = TypeEnv M.empty
                            }

-- | Get a fresh new type variable to use
newTypeVar :: String -> Inferrer Type
newTypeVar prefix = do
  -- get the current state
  s <- get
  -- increment the inferSupply
  put s { inferSupply = inferSupply s + 1 }
  -- wrap it in a type variable and return it
  return $ TVar $ prefix ++ show (inferSupply s)

instantiate :: Scheme -> Inferrer Type
instantiate (Scheme vars t) = do
  newVars <- mapM (\_ -> newTypeVar "a") vars
  -- make a substitution mapping all of the variables in the scheme
  -- to new variables we just generated
  let s = M.fromList (zip vars newVars)
  return $ applySub s t

unify :: Type -> Type -> Inferrer Substitutions
a `unify` b = case (a,b) of
  (l :=> r, l' :=> r') -> do
    s1 <- l `unify` l'
    s2 <- applySub s1 r `unify` applySub s1 r'
    return (s1 `composeSubs` s2)
  (TVar u, t) -> u `bind` t
  (t, TVar u) -> u `bind` t
  (NumberType, NumberType) -> return noSubstitutions
  (StringType, StringType) -> return noSubstitutions
  (t1, t2) -> throwError $ "types do not unify: " ++ show t1 ++ ", " ++ show t2
  where
    bind :: String -> Type -> Inferrer Substitutions
    bind u t | t == TVar u = return noSubstitutions
                | u `S.member` free t =
                  throwError $ "occur check fails: " ++ u ++ ", " ++ show t
                | otherwise = return (M.singleton u t)

infer :: TypeEnv -> Expr -> Inferrer (Substitutions, Type)
infer env@(TypeEnv e) expr = case expr of
  -- symbols are same as variables in this context
  Symbol s -> infer env (Var s)
  Var n -> case M.lookup n e of
    Nothing -> throwError $ "unbound variable: " ++ n
    Just sigma -> do
      t <- instantiate sigma
      return (noSubstitutions, t)
  String _ -> return (noSubstitutions, StringType)
  Number _ -> return (noSubstitutions, NumberType)
  Lambda n e -> do
    tv <- newTypeVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `M.union` M.singleton n (Scheme [] tv))
    (s1, t1) <- infer env'' e
    return (s1, applySub s1 tv :=> t1)
  Apply e1 e2 -> do
    tv <- newTypeVar "a"
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (applySub s1 env) e2
    s3 <- unify (applySub s2 t1) (t2 :=> tv)
    return (s3 `composeSubs` s2 `composeSubs` s1, applySub s3 tv)
  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let TypeEnv env' = remove env x
        t' = generalize (applySub s1 env) t1
        env'' = TypeEnv (M.insert x t' env')
    case e2 of
      Nothing -> return (s1, TupleType [])
      Just e2 -> do
        (s2, t2) <- infer (applySub s1 env'') e2
        return (s1 `composeSubs` s2, t2)

typeInference :: M.Map Name Scheme -> Expr -> Inferrer Type
typeInference env e = uncurry applySub <$> infer (TypeEnv env) e

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
  putStrLn $ "Test " ++ show i ++ " '" ++ show e ++ "'"
  (res, _) <- runInferrer (typeInference M.empty e)
  case res of
    Left err -> putStrLn $ "error: " ++ err
    Right t  -> putStrLn $ show e ++ " : " ++ show t ++ "\n"

main :: IO ()
main = mapM_ testInfer $ zip [0..] [e0, e1, e2, e3, e4, e5]


instance Show Type where
    showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType (TVar n) = PP.text n
prType NumberType = PP.text "Int"
prType StringType = PP.text "Bool"
prType (t :=> s) = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: Type -> PP.Doc
prParenType t = case t of
                      _ :=> _ -> PP.parens (prType t)
                      _ -> prType t

instance Show Scheme where
  showsPrec _ x = shows (prScheme x)

prScheme :: Scheme -> PP.Doc
prScheme (Scheme vars t) = PP.text "All" PP.<+>
                              PP.hcat
                                (PP.punctuate PP.comma (map PP.text vars))
                              PP.<> PP.text "." PP.<+> prType t


test' :: Expr -> IO ()
test' e = do
  (res, _) <- runInferrer (bu S.empty e)
  case res of
    Left err -> putStrLn $ "error: " ++ err
    Right t  -> putStrLn $ show e ++ " :: " ++ show t

data Constraint =
  CEquivalent Type Type
  | CExprlicitInstance Type Scheme
  | CImplicitInstance Type (S.Set String) Type

instance Show Constraint where
  showsPrec _ x = shows (prConstraint x)

prConstraint :: Constraint -> PP.Doc
prConstraint (CEquivalent t1 t2) = PP.hsep [prType t1, PP.text "=", prType t2]
prConstraint (CExprlicitInstance t s) =
    PP.hsep [prType t, PP.text "<~", prScheme s]
prConstraint (CImplicitInstance t1 m t2) = PP.hsep [prType t1,
              PP.text "<=" PP.<>
                PP.parens
                  (PP.hcat
                    (PP.punctuate PP.comma (map PP.text (S.toList m)))),
             prType t2]

type Assum = [(String, Type)]
type CSet = [Constraint]

bu :: S.Set String -> Expr -> Inferrer (Assum, CSet, Type)
bu m (Var n) = do
  b <- newTypeVar "b"
  return ([(n, b)], [], b)
bu m (Number _) = do
  b <- newTypeVar "b"
  return ([], [CEquivalent b NumberType], b)
bu m (String _) = do
  b <- newTypeVar "b"
  return ([], [CEquivalent b StringType], b)
bu m (Apply e1 e2) = do
  (a1, c1, t1) <- bu m e1
  (a2, c2, t2) <- bu m e2
  b <- newTypeVar "b"
  return (a1 ++ a2, c1 ++ c2 ++ [CEquivalent t1 (t2 :=> b)], b)
bu m (Lambda x body) = do
  b@(TVar vn) <- newTypeVar "b"
  (a, c, t) <- bu (vn `S.insert` m) body
  return (a `removeAssum` x, c ++ [CEquivalent t' b | (x', t') <- a, x == x'],
          b :=> t)
bu m (Let x e1 e2) = do
  (a1, c1, t1) <- bu m e1
  case e2 of
    Just e2 -> do
      (a2, c2, t2) <- bu (x `S.delete` m) e2
      return (a1 ++ removeAssum a2 x, c1 ++ c2 ++ [CImplicitInstance t' m t1 |
                                (x', t') <- a2, x' == x], t2)
    Nothing -> error "whoops"

removeAssum [] _ = []
removeAssum ((n', _) : as) n | n == n' = removeAssum as n
removeAssum (a:as) n = a : removeAssum as n
