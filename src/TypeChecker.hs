{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
module TypeChecker (Expr(..),
                   Type(..),
                   infer,
                   test) where

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Error
import Control.Monad.State
import Data.Monoid
import qualified Text.PrettyPrint as PP
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Parser
import Common
import AST
import Prelude hiding (foldr)
import Types
import Defaults

generalize :: TypeMap -> Type -> Polytype
generalize env t = Polytype vars' t'
  where
    -- grab only the variables that are bound in the scope of this type
    vars = S.toList (free t S.\\ free env)
    -- replace them with letters of the alphabet
    vars' = snd <$> zip vars (pure <$> ['a'..])
    -- make sure we get any type classes
    types = map (\v -> TVar (getClasses v t) v) vars
    -- this function will make a tuple mapping the old name to the new
    newType (TVar classes name) new = (name, TVar classes [new])
    -- create the substitutions
    subs = M.fromList $ zipWith newType types ['a'..]
    -- apply the substitutions
    t' = applySub subs t


type Inferrer = ErrorT String (StateT InferrerState IO)
type TypeCheckError = String

data TypeRecord = Checked Polytype
                | Declared Type
                deriving (Show)

type Record = M.Map Name TypeRecord

instance Render Record where
  render _ rec = renderMap rec

instance Render TypeRecord where
  render _ (Checked ptype) = render 0 ptype
  render _ (Declared _type) = render 0 _type

data InferrerState =
  InferrerState { inferSupply :: Name
                , nameStack   :: [Name]
                , records   :: Record
                , typeClasses :: M.Map Name TypeClass
                , instances :: M.Map Name (S.Set Type)
                } deriving (Show)

runInferrer :: Inferrer a -> IO (Either TypeCheckError a, InferrerState)
runInferrer t = runStateT (runErrorT t) initialState
  where initialState = InferrerState { inferSupply = "a0"
                                      , nameStack = ["(root)"]
                                      , records = mempty
                                      , typeClasses = defaultTypeClasses
                                      , instances = defaultInstances }

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
    (TVar [] u, t) -> u `bind` t
    (t, TVar [] u) -> u `bind` t
    (TVar classNames u, t) -> do
      --prnt $ "seeing if " ++ render 0 t ++ " implements " ++ show classNames
      res <- t `implements` classNames
      case res of
        True -> u `bind` t
        False -> throwError $ "Type class unification error: `" ++ render 0 t ++
                  "` does not implement type classes " ++ show classNames
    (TConst n, TConst n') | n == n' -> return mempty
    (TTuple ts1, TTuple ts2) -> mconcat <$> mapM (uncurry unify) (zip ts1 ts2)
    (l :=> r, l' :=> r') -> do
      s1 <- l `unify` l'
      s2 <- applySub s1 r `unify` applySub s1 r'
      return (s1 • s2)
    (l `TApply` r, l' `TApply` r') -> unify (l :=> r) (l' :=> r')
    (t1, t2) -> throwError' $ "types do not unify: " ++ render 0 t1 ++ " !: " ++
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
           else throwError' $ "Type cycle: `" ++ name ++ "` is a free variable " ++
              "with respect to type " ++ render 0 typ

throwError' :: String -> Inferrer a
throwError' message = do
  ns <- getNS
  throwError (message ++ ". When evaluating " ++ ns)

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
    --prnt $ "env was " ++ show env
    (vars, paramT) <- inferPattern env pattern
    --prnt $ "inferring pattern " ++ render 0 pattern ++ " gave vars " ++ show vars
    let env' = M.union (bare <$> vars) env
    --prnt $ "env is now " ++ show env'
    (subs, bodyT) <- infer env' body
    --prnt $ "LAMBDA inferred the body type of " ++ render 0 body ++ " to be " ++ render 0 bodyT
    --prnt $ "LAMBDA returning " ++ render 0 (applySub subs paramT :=> bodyT)
    --prnt $ "this means " ++ render 0 expr ++ " is of type " ++ (render 0 $ applySub subs (applySub subs paramT :=> bodyT))
    return (subs, applySub subs paramT :=> bodyT)
  Apply func arg -> do
    resultT <- newvar
    (funcS, funcT) <- infer env func
    --prnt $ "infered func " ++ render 0 func ++ " : " ++ render 0 funcT
    (argS, argT) <- infer (applySub funcS env) arg
    --prnt $ "infered arg " ++ render 0 arg ++ " : " ++ render 0 argT
    unifyS <- do
      unify (applySub argS funcT) (argT :=> resultT)
      `catchError`
      \msg -> do
        let msg' = concat [ "Type unification error: ", msg
                          , "\nOccurred while evaluating `"
                          , render 0 expr, "`\nThe left-hand side of "
                          , "this expression has type `", render 0 funcT
                          , "`, while the right-hand side has type `"
                          , render 0 argT, "`."]
        throwError msg'

    --prnt $ "unify produced subs: " ++ render 0 unifyS
    --prnt $ "the unified type of " ++ render 0 expr ++ " is " ++ render 0 (applySub unifyS resultT)
    return (unifyS • argS • funcS, applySub unifyS resultT)
  Let name expr' next -> do
    --prnt $ "infering `" ++ render 0 expr ++ "`"
    -- create a new variable for the name
    newT <- nsLookup name >>= \case
      Nothing -> newvar
      Just (Declared t) -> return t
      Just (Checked t) ->
        -- don't allow duplicate definitions
        throwError' $ "Redefinition of `" ++ name ++ "`, which had " ++
        "previously been defined in this scope (with type `" ++
        render 0 t ++ "`)"
    --prnt $ "created a new var " ++ show newT ++ " for " ++ name
    -- create a new environment with that mapping added
    let env1 = M.insert name (bare newT) env
    --prnt $ "env1 is " ++ render 0 env1
    -- infer expr with that environment
    (exprSubs, exprT) <- pushNS name >> infer env1 expr' >>== popNS
    --prnt $ "inferring `" ++ render 0 expr' ++ "` gave us " ++ render 0 exprT ++ ", which after subs is " ++ (render 0 $ applySub exprSubs exprT)
    let exprT' = applySub exprSubs exprT
    subs <- unify newT exprT'
    --prnt $ "unifying " ++ render 0 newT ++ " with " ++ (render 0 $ exprT') ++ " gave us " ++ render 0 subs
    -- generalize the type with respect to previous env
    --prnt $ "generalizing with respect to env " ++ render 0 (applySub (exprSubs) env) ++ " which has free variables " ++ show (free (applySub exprSubs env)) ++ " while exprT has free variables " ++ show (free exprT')
    let genT = generalize (applySub (subs • exprSubs) env) exprT'
    -- create a new environment with that generalized type
        env2 = applySub exprSubs $ M.insert name genT env
    -- make a record of this type
    register name (Checked genT)
    --prnt $ "that type generalizes to " ++ render 0 genT ++ "; this will be assigned into variable `" ++ name ++ "`"
    case next of
      Nothing -> return (exprSubs, tuple [])
      Just next -> do
        -- apply whatever substitutions were produced from evaluating `expr`,
        -- infer the next guy, and compose their substitutions
        --prnt $ "inferring the next expr with env " ++ show env2
        (nextSubs, nextT) <- infer env2 next
        --prnt $ "next we found " ++ show (nextSubs, nextT)
        return (exprSubs • nextSubs, nextT)
  Sig name typ next -> do
    nsLookup name >>= \case
      Just (Checked t) -> throwError' $ "Redeclaration of variable `" ++ name ++ "`"
      Just (Declared t) -> throwError' $ "Redeclaration of variable `" ++ name ++ "`"
      Nothing -> register name (Declared typ) >> case next of
        Nothing -> only (tuple [])
        Just expr -> infer env expr
  otherwise -> throwError' $ "Unhandleable expression " ++ prettyExpr expr
  where only t = return (mempty, t)
        newvar = newTypeVar []
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

prnt :: String -> Inferrer ()
prnt = lift . lift . putStrLn

typeInference :: M.Map Name Polytype -> Expr -> Inferrer Type
typeInference env e = uncurry applySub <$> infer env e

test s = do
  expr <- grab s
  (res, env) <- runInferrer (typeInference defaultTypeMap (desugar expr))
  case res of
    Left err -> putStrLn $ "error: " ++ err ++ "\n"
    Right t  -> putStrLn $ "Expr: " ++ render 0 expr ++
                           "\nType: " ++ render 0 t ++ "\n" ++
                           "Env:  " ++ render 0 (records env)

-- | @implements@ checks if the list of type classes is implemented by
-- the type given.
implements :: Type -> [Name] -> Inferrer Bool
typ `implements` classNames = do
  --prnt $ "Seeing if " ++ render 0 typ ++ " implements " ++ show classNames
  results <- mapM check classNames
  return $ all (== True) results where
  check className = case typ of
    -- an unrestricted type variable will always match with any type
    TVar [] _ -> return True
    TVar classNames' _ -> search classNames' className
    otherwise -> do
      --prnt $ "here we are"
      res <- typ `isInstance` className
      --prnt $ "Result we got was " ++ show res
      return res

-- @search@ recurses through the list of names; if it finds itself it returns
-- true; otherwise it gets the parents of all the types and recurses on that.
-- If it runs out of names to look through it returns false.
search :: [Name] -> Name -> Inferrer Bool
search classNames className = case classNames of
    -- if we've run out of names, then we didn't find ourselves
    [] -> return False
    -- if this class is an element of classes', then we're golden
    _ | className `elem` classNames -> return True
    -- otherwise, we need to get all of their parents
      | otherwise -> do
        parents <- concat <$> mapM getParents classNames
        search parents className
  where
    -- @getParents@ is a wrapper for looking up the class and calling inherits
    getParents :: Name -> Inferrer [Name]
    getParents className = get <!> typeClasses <!> M.lookup className >>= \case
      Nothing -> error $ "FATAL: type class `" ++ className ++ "` wasn't found"
      Just tclass -> return $ inherits tclass

-- | @isInstance@ checks if a list of types make up an instance of a type
-- class. It looks the set of instances up in the @instances@ dictionary and
-- iterates through it, asking if the types are a match for any of the instances.
isInstance :: Type -> Name -> Inferrer Bool
typ `isInstance` className = do
  --prnt $ "Seeing if " ++ render 0 typ ++ " is an instance of " ++ className
  get <!> instances <!> M.lookup className >>= \case
    Nothing -> error $ "Invalid type class " ++ show className
    Just types -> any (== True) <$> mapM (isMatch typ) (S.toList types)

isMatch :: Type -> Type -> Inferrer Bool
tester `isMatch` stored = do
  --prnt $ "Seeing if " ++ render 0 tester ++ " is a match for " ++ render 0 stored
  case (tester, stored) of
    -- if what we have stored is a variable, we can just use @implements@
    (tester, TVar classNames _) -> tester `implements` classNames
    (TConst n, TConst n') -> do
      --when (n == n') (prnt $ "w00t!")
      return $ n == n'
    (TApply a b , TApply c d) -> do
      res1 <- (a `isMatch` c)
      res2 <- (b `isMatch` d)
      return $ res1 == res2
    (TTuple ts, TTuple ts') -> and <$> zipWithM isMatch ts ts'
    (a :=> b, c :=> d) -> isMatch (TApply a b) (TApply c d)
    (a, b) -> return False
