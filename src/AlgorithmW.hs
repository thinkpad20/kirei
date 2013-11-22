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
  | FunctionType Type Type
  deriving (Eq, Ord)

data Scheme = Scheme [String] Type

class Types a where
  freeTypeVars :: a -> S.Set String
  applySub :: Substitutions -> a -> a

instance Types Type where
  freeTypeVars (TVar n) = S.singleton n
  freeTypeVars NumberType = S.empty
  freeTypeVars StringType = S.empty
  freeTypeVars (FunctionType t1 t2) = freeTypeVars t1 `S.union` freeTypeVars t2

  applySub s (TVar n) = case M.lookup n s of
    Nothing  -> TVar n
    Just t   -> t
  applySub s (FunctionType t1 t2) = FunctionType (applySub s t1) (applySub s t2)
  applySub s t = t

instance Types Scheme where
  freeTypeVars (Scheme vars t) =
    (freeTypeVars t) `S.difference` (S.fromList vars)
  applySub s (Scheme vars t) =
    Scheme vars (applySub (foldr M.delete s vars) t)

instance Types a => Types [a] where
  applySub s  =  map (applySub s)
  freeTypeVars l    =  foldr S.union S.empty (map freeTypeVars l)

type Substitutions = M.Map String Type

nullSubs  ::  Substitutions
nullSubs  =   M.empty

composeSubs :: Substitutions -> Substitutions -> Substitutions
composeSubs s1 s2 = (M.map (applySub s1) s2) `M.union` s1

newtype TypeEnv = TypeEnv (M.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

instance Types TypeEnv where
  freeTypeVars (TypeEnv env) = freeTypeVars (M.elems env)
  applySub s (TypeEnv env) = TypeEnv (M.map (applySub s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = S.toList ((freeTypeVars t) `S.difference` (freeTypeVars env))


data InferrerEnv = InferrerEnv (M.Map String Type)

data InferrerState =
  InferrerState {
    inferSupply :: Int,
    inferSubstitutions :: Substitutions
  }

type Inferrer a =
  ErrorT String (ReaderT InferrerEnv (StateT InferrerState IO)) a

runInferrer :: Inferrer a -> IO (Either String a, InferrerState)
runInferrer t =
    runStateT (runReaderT (runErrorT t) initInferrerEnv) initInferrerState
  where initInferrerEnv = InferrerEnv M.empty
        initInferrerState = InferrerState {
                              inferSupply = 0,
                              inferSubstitutions = M.empty
                            }

newTyVar :: String -> Inferrer Type
newTyVar prefix = do
  s <- get
  put s { inferSupply = inferSupply s + 1 }
  return (TVar (prefix ++ show (inferSupply s)))

instantiate :: Scheme -> Inferrer Type
instantiate (Scheme vars t) = do
  nvars <- mapM (\ _ -> newTyVar "a") vars
  let s = M.fromList (zip vars nvars)
  return $ applySub s t

mgu :: Type -> Type -> Inferrer Substitutions
mgu (FunctionType l r) (FunctionType l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (applySub s1 r) (applySub s1 r')
  return (s1 `composeSubs` s2)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu NumberType NumberType = return nullSubs
mgu StringType StringType = return nullSubs
mgu t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ ", " ++ show t2

varBind :: String -> Type -> Inferrer Substitutions
varBind u t | t == TVar u = return nullSubs
            | u `S.member` freeTypeVars t =
              throwError $ "occur check fails: " ++ u ++ ", " ++ show t
            | otherwise = return (M.singleton u t)

infer :: TypeEnv -> Expr -> Inferrer (Substitutions, Type)
infer (TypeEnv env) (Var n) = case M.lookup n env of
  Nothing -> throwError $ "unbound variable: " ++ n
  Just sigma -> do
    t <- instantiate sigma
    return (nullSubs, t)
infer env (String _) = return (nullSubs, StringType)
infer env (Number _) = return (nullSubs, NumberType)
infer env (Lambda n e) = do
  tv <- newTyVar "a"
  let TypeEnv env' = remove env n
      env'' = TypeEnv (env' `M.union` M.singleton n (Scheme [] tv))
  (s1, t1) <- infer env'' e
  return (s1, FunctionType (applySub s1 tv) t1)
infer env (Apply e1 e2) = do
  tv <- newTyVar "a"
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applySub s1 env) e2
  s3 <- mgu (applySub s2 t1) (FunctionType t2 tv)
  return (s3 `composeSubs` s2 `composeSubs` s1, applySub s3 tv)
infer env (Let x e1 e2) = do
  (s1, t1) <- infer env e1
  let TypeEnv env' = remove env x
      t' = generalize (applySub s1 env) t1
      env'' = TypeEnv (M.insert x t' env')
  case e2 of
    Nothing -> return (s1, TupleType [])
    Just e2 -> do
      (s2, t2) <- infer (applySub s1 env'') e2
      return (s1 `composeSubs` s2, t2)

typeInference :: M.Map String Scheme -> Expr -> Inferrer Type
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
    Left err  ->  putStrLn $ "error: " ++ err
    Right t   ->  putStrLn $ show e ++ " : " ++ show t ++ "\n"

main :: IO ()
main = mapM_ testInfer $ zip [0..] [e0, e1, e2, e3, e4, e5]


instance Show Type where
    showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType (TVar n) = PP.text n
prType NumberType = PP.text "Int"
prType StringType = PP.text "Bool"
prType (FunctionType t s) = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: Type -> PP.Doc
prParenType t = case t of
                      FunctionType _ _ -> PP.parens (prType t)
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
  b <- newTyVar "b"
  return ([(n, b)], [], b)
bu m (Number _) = do
  b <- newTyVar "b"
  return ([], [CEquivalent b NumberType], b)
bu m (String _) = do
  b <- newTyVar "b"
  return ([], [CEquivalent b StringType], b)
bu m (Apply e1 e2) = do
  (a1, c1, t1) <- bu m e1
  (a2, c2, t2) <- bu m e2
  b <- newTyVar "b"
  return (a1 ++ a2, c1 ++ c2 ++ [CEquivalent t1 (FunctionType t2 b)], b)
bu m (Lambda x body) = do
  b@(TVar vn) <- newTyVar "b"
  (a, c, t) <- bu (vn `S.insert` m) body
  return (a `removeAssum` x, c ++ [CEquivalent t' b | (x', t') <- a, x == x'],
          FunctionType b t)
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
