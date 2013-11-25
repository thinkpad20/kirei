module HM  (  Expr(..),
              Type(..),
              infer,
              test
           ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Text.PrettyPrint as PP
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Parser

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

instance Types Polytype where
  free (Polytype vars t) =
    (free t) `S.difference` (S.fromList vars)
  applySub s (Polytype vars t) =
    Polytype vars (applySub (foldr M.delete s vars) t)

instance Types a => Types [a] where
  applySub s = map (applySub s)
  free l = foldr (∪) (∅) (map free l)

type Substitutions = M.Map String Type

noSubstitutions :: Substitutions
noSubstitutions = M.empty

(=>=) :: Substitutions -> Substitutions -> Substitutions
s1 =>= s2 = (applySub s1 <$> s2) `M.union` s1

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) var = TypeEnv (M.delete var env)

instance Types TypeEnv where
  free (TypeEnv env) = free (M.elems env)
  applySub s (TypeEnv env) = TypeEnv (applySub s <$> env)

generalize :: TypeEnv -> Type -> Polytype
generalize env t = Polytype vars t
  where vars = S.toList (free t `S.difference` free env)


data InferrerEnv = InferrerEnv (M.Map String Type)

data InferrerState =
  InferrerState {
    inferSupply :: Int,
    inferSubstitutions :: Substitutions,
    namesToTypes :: M.Map Name Type,
    namesToPolytypes :: TypeEnv
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
                              namesToPolytypes = TypeEnv M.empty
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

instantiate :: Polytype -> Inferrer Type
instantiate s@(Polytype vars t) = do
  newVars <- mapM (\_ -> newTypeVar "a") vars
  -- make a substitution mapping all of the variables in the scheme
  -- to new variables we just generated
  let s = M.fromList (zip vars newVars)
  return $ applySub s t

unify :: Type -> Type -> Inferrer Substitutions
a `unify` b = do
  case (a,b) of
    (l :=> r, l' :=> r') -> do
      s1 <- l `unify` l'
      s2 <- applySub s1 r `unify` applySub s1 r'
      return (s1 =>= s2)
    (TVar u, t) -> u `bind` t
    (t, TVar u) -> u `bind` t
    (NumberType, NumberType) -> return noSubstitutions
    (StringType, StringType) -> return noSubstitutions
    (t1, t2) -> throwError $ "types do not unify: " ++ show t1 ++ ", " ++ show t2
  where
    bind :: Name -> Type -> Inferrer Substitutions
    bind name typ | typ == TVar name = return noSubstitutions
                  | name `S.member` free typ = throwError $
                      "Error: " ++ name ++ " occurs free in type " ++ show typ
                  | otherwise = do
                    return (M.singleton name typ)

infer :: TypeEnv -> Expr -> Inferrer (Substitutions, Type)
infer env@(TypeEnv tenv) expr = case expr of
  String _ -> noSubs StringType
  Number _ -> noSubs NumberType
  Bool _ -> noSubs BoolType
  -- symbols are same as variables in this context
  Symbol s -> infer env (Var s)
  Var n -> case M.lookup n tenv of
    Nothing -> throwError $ "Unknown variable: " ++ n
    Just sigma -> noSubs =<< instantiate sigma
  Lambda param body -> do
    paramT <- newTypeVar "a"
    -- set the param variable to point to this new type
    let env' = TypeEnv $ M.insert param (Polytype [] paramT) tenv
    -- infer the body of the function with this new environment
    (subs, returnT) <- infer env' body
    return (subs, applySub subs paramT :=> returnT)
  Apply e1 e2 -> do
    tv <- newTypeVar "a"
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (applySub s1 env) e2
    s3 <- unify (applySub s2 t1) (t2 :=> tv)
    return (s3 =>= s2 =>= s1, applySub s3 tv)
  Let var expr next -> do
    (exprSubs, exprT) <- infer env expr
    -- remove any existing association this variable has
    let TypeEnv env' = remove env var
    -- get the variable
        varT = generalize (applySub exprSubs env) exprT
        env'' = TypeEnv (M.insert var varT env')
    case next of
      Nothing -> return (exprSubs, TupleType [])
      Just next -> do
        -- apply whatever substitutions were produced from evaluating `expr`,
        -- infer the next guy, and compose their substitutions
        (nextSubs, nextT) <- infer (applySub exprSubs env'') next
        return (exprSubs =>= nextSubs, nextT)
  If c t f -> do
    (s1, condT) <- infer env c
    (s2, trueT) <- infer (applySub s1 env) t
    (s3, falseT) <- infer (applySub (s1 =>= s2) env) f
    s4 <- unify trueT falseT
    return (s1 =>= s2 =>= s3 =>= s4, falseT)

  where noSubs t = return (noSubstitutions, t)

typeInference :: M.Map Name Polytype -> Expr -> Inferrer Type
typeInference env e = uncurry applySub <$> infer (TypeEnv env) e

test s = do
  let e = grab s
  (res, _) <- runInferrer (typeInference M.empty e)
  case res of
    Left err -> putStrLn $ "error: " ++ err ++ "\n"
    Right t  -> putStrLn $ "Expr: " ++ prettyExpr e ++ "\nType: " ++ show t ++ "\n\n"

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
  putStrLn $ "Test " ++ show i ++ ":\n\t'" ++ prettyExpr e ++ "'\n"
  (res, _) <- runInferrer (typeInference M.empty e)
  case res of
    Left err -> putStrLn $ "error: " ++ err ++ "\n"
    Right t  -> putStrLn $ prettyExpr e ++ " : " ++ show t ++ "\n\n"

testIt = main

main :: IO ()
main = mapM_ testInfer $ zip [0..] [e0, e1, e2, e3, e4, e5]


instance Show Type where
    showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType (TVar n) = PP.text n
prType NumberType = PP.text "Number"
prType StringType = PP.text "Bool"
prType (t :=> s) = prParenType t PP.<+> PP.text "->" PP.<+> prType s

prParenType :: Type -> PP.Doc
prParenType t = case t of
                      _ :=> _ -> PP.parens (prType t)
                      _ -> prType t

instance Show Polytype where
  showsPrec _ x = shows (prPolytype x)

prPolytype :: Polytype -> PP.Doc
prPolytype (Polytype vars t) = case vars of
  [] -> prType t
  _  -> PP.text "∀" PP.<+>
          PP.hcat
            (PP.punctuate PP.comma (map PP.text vars))
            PP.<> PP.text "." PP.<+> prType t


test' :: Expr -> IO ()
test' e = do
  (res, _) <- runInferrer (bu S.empty e)
  case res of
    Left err -> putStrLn $ "error: " ++ err
    Right t  -> putStrLn $ prettyExpr e ++ " :: " ++ show t

data Constraint =
  CEquivalent Type Type
  | CExprlicitInstance Type Polytype
  | CImplicitInstance Type (S.Set String) Type

instance Show Constraint where
  showsPrec _ x = shows (prConstraint x)

prConstraint :: Constraint -> PP.Doc
prConstraint (CEquivalent t1 t2) = PP.hsep [prType t1, PP.text "=", prType t2]
prConstraint (CExprlicitInstance t s) =
    PP.hsep [prType t, PP.text "<~", prPolytype s]
prConstraint (CImplicitInstance t1 m t2) = PP.hsep [prType t1,
              PP.text "<=" PP.<>
                PP.parens
                  (PP.hcat
                    (PP.punctuate PP.comma (map PP.text (S.toList m)))),
             prType t2]

type Assumptions = [(String, Type)]
type CSet = [Constraint]

bu :: S.Set String -> Expr -> Inferrer (Assumptions, CSet, Type)
bu m (Symbol s) = bu m (Var s)
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
