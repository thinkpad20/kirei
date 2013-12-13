module AlgorithmW  (Expr(..),
                    Type(..),
                    infer,
                    test) where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid
import qualified Text.PrettyPrint as PP
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Parser
import Common
import AST
import Prelude hiding (foldr)
import Types hiding (FreeVars(..))

generalize :: TypeMap -> Type -> Polytype
generalize env t = Polytype vars t
  where vars = S.toList (free t S.\\ free env)


data InferrerEnv = InferrerEnv (M.Map String Type)

data InferrerState = InferrerState { inferSupply :: Name }

type Inferrer = ErrorT String (ReaderT InferrerEnv (StateT InferrerState IO))

runInferrer :: Inferrer a -> IO (Either String a, InferrerState)
runInferrer t =
    runStateT (runReaderT (runErrorT t) initInferrerEnv) initInferrerState
  where initInferrerEnv = InferrerEnv M.empty
        initInferrerState = InferrerState { inferSupply = "b" }

-- | Get a fresh new type variable to use
newTypeVar :: String -> Inferrer Type
newTypeVar prefix = do
  -- get the current state
  s <- get
  -- increment the inferSupply
  modify $ \s -> s { inferSupply = next (inferSupply s) }
  -- wrap it in a type variable and return it
  return $ TVar $ inferSupply s
  where
    next name = let (c:cs) = reverse name in
      if c < 'z' then reverse $ succ c : cs else 'a' : (map (\_ -> 'a') name)

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
      return (s1 • s2)
    (l `TApply` r, l' `TApply` r') -> unify (l :=> r) (l' :=> r')
    (TVar u, t) -> u `bind` t
    (t, TVar u) -> u `bind` t
    (TConst n, TConst n') | n == n' -> return noSubstitutions
    (t1, t2) -> throwError $ "types do not unify: " ++ show t1 ++ ", " ++ show t2
  where
    bind :: Name -> Type -> Inferrer Substitutions
    bind name typ =
      if typ == TVar name
      then return noSubstitutions
      else if not $ name `S.member` free typ
           then return (M.singleton name typ)
           -- what does this error mean? well let's say we're binding `a` to
           -- `(b :=> a)`. Well in the type `(b :=> a)`, one of the free variables
           -- is the type `a`. This constitutes a circular type definition, since
           -- if we tried to apply the substitution, we'd be back to where we
           -- started, with the type `a` still hanging around. So an occurs check
           -- here is checking if this substitution is meaningful or not, which
           -- has the dual meaning of checking if we're trying to construct the
           -- infinite type.
           else throwError $ "Error: `" ++ name ++ "` is a free variable " ++
              "with respect to type " ++ render 0 typ

infer :: TypeMap -> Expr -> Inferrer (Substitutions, Type)
infer env@(TM tenv) expr = case expr of
  String _ -> noSubs str
  Number _ -> noSubs num
  Bool _ -> noSubs bool
  -- symbols are same as variables in this context
  Symbol s -> infer env (Var s)
  Var n -> case M.lookup n tenv of
    Nothing -> throwError $ "Unknown variable: " ++ n
    Just sigma -> instantiate sigma >>= noSubs
  TypeName n -> infer env (Var n)
  Lambda pattern body -> do
    --prnt $ "env was " ++ show env
    (vars, paramT) <- inferPattern env pattern
    --prnt $ "inferring pattern " ++ render 0 pattern ++ " gave vars " ++ show vars
    let env' = tmUnion (TM $ (bare <$> vars)) env
    --prnt $ "env is now " ++ show env'
    (subs, bodyT) <- infer env' body
    prnt $ "LAMBDA inferred the body type of " ++ render 0 body ++ " to be " ++ render 0 bodyT
    prnt $ "LAMBDA returning " ++ render 0 (applySub subs paramT :=> bodyT)
    prnt $ "this means " ++ render 0 expr ++ " is of type " ++ (render 0 $ applySub subs (applySub subs paramT :=> bodyT))
    return (subs, applySub subs paramT :=> bodyT)
  Apply e1 e2 -> do
    tv <- newvar
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (applySub s1 env) e2
    s3 <- unify (applySub s2 t1) (t2 :=> tv)
    prnt $ "the unified type of " ++ show expr ++ " is " ++ show (applySub s3 tv)
    return (s3 • s2 • s1, applySub s3 tv)
  Let name expr' next -> do
    prnt $ "infering `" ++ render 0 expr ++ "`"
    -- create a new variable for the name
    newT <- newvar
    prnt $ "created a new var " ++ show newT ++ " for " ++ name
    -- create a new environment with that mapping added
    let env1 = tmInsert name (bare newT) env
    prnt $ "env1 is " ++ render 0 env1
    -- infer expr with that environment
    (exprSubs, exprT) <- infer env1 expr'
    prnt $ "inferring `" ++ render 0 expr' ++ "` gave us " ++ render 0 exprT ++ ", which after subs is " ++ (render 0 $ applySub exprSubs exprT)
    let exprT' = applySub exprSubs exprT
    subs <- unify newT exprT'
    prnt $ "unifying " ++ render 0 newT ++ " with " ++ (render 0 $ exprT') ++ " gave us " ++ render 0 subs
    -- generalize the type with respect to previous env
    prnt $ "generalizing with respect to env " ++ render 0 (applySub (exprSubs) env) ++ " which has free variables " ++ show (free (applySub exprSubs env)) ++ " while exprT has free variables " ++ show (free exprT')
    let genT = generalize (applySub (subs • exprSubs) env) exprT'
    -- create a new environment with that generalized type
        env2 = applySub exprSubs $ tmInsert name genT env
    prnt $ "that type generalizes to " ++ render 0 genT ++ "; this will be assigned into variable `" ++ name ++ "`"
    prnt $ "env has " ++ render 0 env
    prnt $ "env1 has " ++ render 0 env1
    prnt $ "env2 has " ++ render 0 env2
    case next of
      Nothing -> return (exprSubs, tuple [])
      Just next -> do
        -- apply whatever substitutions were produced from evaluating `expr`,
        -- infer the next guy, and compose their substitutions
        prnt $ "inferring the next expr with env " ++ show env2
        (nextSubs, nextT) <- infer env2 next
        prnt $ "next we found " ++ show (nextSubs, nextT)
        return (exprSubs • nextSubs, nextT)
  If c t f -> do
    (s1, condT) <- infer env c
    (s2, trueT) <- infer (applySub s1 env) t
    (s3, falseT) <- infer (applySub (s1 • s2) env) f
    s4 <- unify trueT falseT
    return (s1 • s2 • s3 • s4, falseT)

  where noSubs t = return (noSubstitutions, t)
        newvar = newTypeVar "a"
        inferPattern env pat = case pat of
          Var v -> do
            newT <- newvar
            return (M.singleton v newT, newT)
          TypeName n -> infer env (Var n)
          Tuple es -> do
            subsAndTypes <- mapM (inferPattern env) es
            let subs' = mconcat (map fst subsAndTypes)
            return (subs', TTuple $ map snd subsAndTypes)
          Apply func arg -> do
            (subs1, funcT) <- inferPattern env func
            (subs2, argT) <- inferPattern env arg
            returnT <- newvar
            subs3 <- unify funcT (argT :=> returnT)
            return (subs3 • subs2 • subs1, applySub subs3 returnT)
          Number _ -> noSubs num
          String _ -> noSubs str
          Bool   _ -> noSubs bool
        prnt :: String -> Inferrer ()
        prnt = lift . lift . lift . putStrLn

typeInference :: M.Map Name Polytype -> Expr -> Inferrer Type
typeInference env e = uncurry applySub <$> infer (TM env) e

test s = do
  expr <- grab s <!> desugar
  (res, _) <- runInferrer (typeInference initials expr)
  case res of
    Left err -> putStrLn $ "error: " ++ err ++ "\n"
    Right t  -> putStrLn $ "Expr: " ++ render 0 expr ++
                           "\nType: " ++ render 0 t ++ "\n\n"

{- TESTING... -}

testInfer :: (Int, Expr) -> IO ()
testInfer (i, e) = do
  putStrLn $ "Test " ++ show i ++ ":\n\t'" ++ prettyExpr e ++ "'\n"
  (res, _) <- runInferrer (typeInference M.empty e)
  case res of
    Left err -> putStrLn $ "error: " ++ err ++ "\n"
    Right t  -> putStrLn $ prettyExpr e ++ " : " ++ render 0 t ++ "\n\n"

testIt = main

main :: IO ()
main = do
  e0 <- grab "let id = \\x -> x; id;"
  e1 <- grab "let id = \\x -> x; id id;"
  e2 <- grab "let id = \\x -> let y = x; y; id id;"
  e3 <- grab "let id = \\x -> let y = x; y; id id 2;"
  -- λx . x x should fail
  e4 <- grab "let id = \\x -> x x; id;"
  e5 <- grab "\\m -> let y = m; let x = y \"hello\"; x;"
  mapM_ testInfer $ zip [0..] [e0, e1, e2, e3, e4, e5]

initials = M.fromList
  [
    --("+", bare $ num :=> num :=> num),
    --("-", bare $ num :=> num :=> num),
    --("*", bare $ num :=> num :=> num),
    --("/", bare $ num :=> num :=> num),
    --("<", bare $ num :=> num :=> num),
    --(">", bare $ num :=> num :=> num),
    --("<=", bare $ num :=> num :=> num),
    --(">=", bare $ num :=> num :=> num),
    --("==", bare $ num :=> num :=> num),
    --("!=", bare $ num :=> num :=> num),
    --("(if)", witha $ bool :=> a :=> a :=> a),
    ("[]", witha $ listT a),
    ("::", witha $ a :=> listT a :=> listT a),
    --("(fail)", witha a),
    ("(error)", witha a),
    ("(or)", witha $ a :=> a :=> a),
    ("(range)", witha $ a :=> a :=> listT a)
  ]
  where witha = Polytype ["a"]
        a = TVar "a"
        b = TVar "b"
        listT = TApply (TConst "[]")
        maybeT = TApply (TConst "Maybe")
