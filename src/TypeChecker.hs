{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module TypeChecker (Expr(..),
                   Type(..),
                   infer,
                   test) where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), (<|>))
import Data.List (intercalate)
import Parser
import Common
import AST
import Prelude hiding (foldr)
import Types
import TypeClass

runInferrer :: Inferrer a -> IO (Either TypeCheckError a, InferrerState)
runInferrer t = runStateT (runErrorT t) initialState
  where initialState = InferrerState { inferSupply = "a0"
                                      , nameStack = ["(root)"]
                                      , records = mempty
                                      , kinds = defaultKinds
                                      , typeClasses = defaultTypeClasses
                                      , instances = defaultInstances }

typeInference :: M.Map Name Polytype -> Expr -> Inferrer Type
typeInference env e = uncurry applySub <$> infer env e

test :: String -> IO ()
test s = do
  expr <- grab s
  (res, env) <- runInferrer (typeInference defaultTypeMap (desugar expr))
  case res of
    Left err -> putStrLn $ "error: " ++ err ++ "\n"
    Right t  -> putStrLn $ "Expr: " ++ render 0 expr ++
                           "\nType: " ++ render 0 t ++ "\n" ++
                           "Env:  " ++ render 0 (records env) ++
                           "TypeClasses" ++ render 0 (typeClasses env)

generalize :: TypeMap -> Type -> Polytype
generalize env t = Polytype vars' t'
  where
    -- grab only the variables that are bound in the scope of this type
    vars = S.toList (free t S.\\ free env)
    -- replace them with letters of the alphabet
    vars' = snd <$> zip vars (pure <$> ['a'..])
    -- collect all of the type classes and construct TVars
    types = map (\v -> TVar (getClasses v t) v) vars
    -- this function will make a tuple mapping the old name to the new
    newType (TVar classes name) new = (name, TVar classes [new])
    -- create the substitutions
    subs = M.fromList $ zipWith newType types ['a'..]
    -- apply the substitutions to the internal type
    t' = applySub subs t

-- | Get a fresh new type variable to use
newTypeVar :: [Name] -> Inferrer Type
newTypeVar classes = do
  -- get the current state
  var <- inferSupply <$> get
  -- increment the inferSupply
  modify $ \s -> s { inferSupply = next var }
  -- wrap it in a type variable and return it
  return $ TVar classes var
  where
    next name = let (c:cs) = reverse name in
      if c < '9' then reverse $ succ c : cs
      else if (head name) < 'z' then (succ $ head name) : "0"
      else map (\_ -> 'a') name ++ "0"

instantiate :: Polytype -> Inferrer Type
instantiate s@(Polytype vars t) = do
  newVars <- mapM (\var -> getClasses var t ! newTypeVar) vars
  -- make a substitution mapping all of the variables in the scheme
  -- to new variables we just generated
  let s = M.fromList (zip vars newVars)
      res = applySub s t
  return res

unify :: Type -> Type -> Inferrer Substitutions
a `unify` b = do
  --prnt $ "Unify called with " ++ render 0 a ++ ", " ++ render 0 b
  case (a,b) of
    (t, TVar classNames u) -> unify b a
    (TVar classNames u, t) -> do
      --prnt $ "seeing if " ++ render 0 t ++ " implements " ++ show classNames
      res <- t `implements` classNames
      case res of
        True -> u `bind` t
        False -> throwError $
          concat [ "Type class unification error: `", render 0 t
                 , "` does not implement "
                 , case classNames of
                    [name] -> "type class " ++ name
                    names -> "type classes " ++ intercalate ", " names]
    (TConst n, TConst n') | n == n' -> return mempty
    (TTuple ts1, TTuple ts2) -> mconcat <$> mapM (uncurry unify) (zip ts1 ts2)
    (l :=> r, l' :=> r') -> do
      s1 <- l `unify` l'
      s2 <- applySub s1 r `unify` applySub s1 r'
      return (s1 • s2)
    (l `TApply` r, l' `TApply` r') -> unify (l :=> r) (l' :=> r')
    (t1, t2) -> throwError $ "types do not unify: " ++ render 0 t1 ++ " !: " ++
                   render 0 t2
  where
    bind :: Name -> Type -> Inferrer Substitutions
    bind name typ =
      if typ == TVar [] name
      then return mempty
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
           else throwError $ concat [ "Type cycle detected when attempting "
                                     , "to unify `", render 0 a, "` with `"
                                     , render 0 b, "`: ", name, "` is a free "
                                     , "variable with respect to type "
                                     , render 0 typ]

throwError' :: String -> Inferrer a
throwError' message = do
  ns <- getNS
  throwError (message ++ "\nOccurred when type checking " ++ ns)

getFull :: Name -> Inferrer Name
getFull name = getNS <!> (++ "." ++ name)

nsLookup :: Name -> Inferrer (Maybe TypeRecord)
nsLookup name = do
  fullname <- getFull name
  get <!> records <!> M.lookup fullname

register :: Name -> TypeRecord -> Inferrer ()
register name rec = do
  fullname <- getFull name
  modify $ \s -> s { records = M.insert fullname rec (records s) }

pushNS :: Name -> Inferrer ()
pushNS name = modify $ \s -> s {nameStack = name : (nameStack s)}

popNS :: Inferrer ()
popNS = modify $ \s -> s {nameStack = tail (nameStack s)}

getNS :: Inferrer Name
getNS = get <!> nameStack <!> reverse <!> intercalate "."

infer :: TypeMap -> Expr -> Inferrer (Substitutions, Type)
infer env expr = case expr of
  String _ -> only str
  Number _ -> only num
  Bool _ -> only bool
  Tuple exprs -> do
    subsAndTypes <- mapM (infer env) exprs
    let subs' = mconcat (map fst subsAndTypes)
    return (subs', TTuple $ map snd subsAndTypes)
  -- symbols are same as variables in this context
  Symbol s -> infer env (Var s)
  Var name -> case M.lookup name env of
    -- if it's not in the env, it might just be declared
    Nothing -> nsLookup name >>= \case
      -- if it hasn't been declared, that's definitely an error
      Nothing -> throwError' $ "Unknown variable: " ++ name
      -- if it's a checked type it should be in the env, but just in case...
      Just (Checked polytype) -> only =<< instantiate polytype
      -- if it's declared, we'll trust it but we need to work some magic on it
      Just (Declared typ) -> do
        -- generalizing will make a polytype with fresh type variables
        let generalized = generalize mempty typ
        -- we then instantiate, which turns it back into a Type
        typ' <- instantiate generalized
        -- and then we can return it with no substitutions
        only typ'
    Just polytype -> only =<< instantiate polytype
  TypeName n -> infer env (Var n)
  Lambda pattern body -> do
    (vars, paramT) <- inferPattern env pattern
    let env' = M.union (bare <$> vars) env
    (subs, bodyT) <- infer env' body
    return (subs, applySub subs paramT :=> bodyT)
  Apply func arg -> do
    resultT <- newvar
    (funcS, funcT) <- infer env func
    (argS, argT) <- infer (applySub funcS env) arg
    unifyS <- do
      unify (applySub argS funcT) (argT :=> resultT)
      `catchError`
      \msg -> do
        let msg' = concat [ "Type unification error:\n", msg
                          , "\nWhile type checking the expression `"
                          , render 0 expr, "`\nThe left-hand side of "
                          , "this expression has type `", render 0 funcT
                          , "`, and the right-hand side has type `"
                          , render 0 argT, "`."]
        throwError msg'
    return (unifyS • argS • funcS, applySub unifyS resultT)
  Let name expr' next -> do
    -- create a new variable for the name
    newT <- nsLookup name >>= \case
      Nothing -> newvar
      Just (Declared t) -> return t
      Just (Checked t) ->
        -- don't allow duplicate definitions
        throwError' $ "Redefinition of `" ++ name ++ "`, which had " ++
        "previously been defined in this scope (with type `" ++
        render 0 t ++ "`)"
    -- create a new environment with that mapping added
    let env1 = M.insert name (bare newT) env
    -- infer expr with that environment
    (exprSubs, exprT) <- pushNS name >> infer env1 expr' >>== popNS
    let exprT' = applySub exprSubs exprT
    subs <- unify newT exprT'
    -- generalize the type with respect to previous env
    let genT = generalize (applySub (subs • exprSubs) env) exprT'
    -- create a new environment with that generalized type
        env2 = applySub exprSubs $ M.insert name genT env
    -- make a record of this type
    register name (Checked genT)
    case next of
      Nothing -> return (exprSubs, tuple [])
      Just next -> do
        -- apply whatever substitutions were produced from evaluating `expr`,
        -- infer the next guy, and compose their substitutions
        (nextSubs, nextT) <- infer env2 next
        return (exprSubs • nextSubs, nextT)
  Sig name typ next -> do
    nsLookup name >>= \case
      Nothing -> register name (Declared typ) >> handle next
      Just _ -> throwError' $ "Redeclaration of variable `" ++ name ++ "`"
  TypeClass name types sigs next -> do
    tclass <- makeTypeClass name types sigs `catchError` handler
    addTypeClass name tclass
    handle next
    where handler msg = throwError' $ concat [ "Invalid declaration for type "
                                             , "class `", name, "`. Error "
                                             , "returned was: ", msg]
  otherwise -> throwError' $ "Unhandleable expression " ++ prettyExpr expr
  where
    only t = return (mempty, t)
    newvar = newTypeVar []
    handle next = case next of
      Nothing -> only (tuple [])
      Just expr -> infer env expr
    addTypeClass name _class = modify $
      \s -> s { typeClasses = M.insert name _class (typeClasses s) }

inferPattern :: TypeMap -> Expr -> Inferrer (Substitutions, Type)
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
  Placeholder -> newvar >>= only
  Number _ -> only num
  String _ -> only str
  Bool   _ -> only bool
  where only t = return (mempty, t)
        newvar = newTypeVar []

prnt :: String -> Inferrer ()
prnt = lift . lift . putStrLn
