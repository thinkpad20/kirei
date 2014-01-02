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
import qualified KindChecker as K

-- Utility/wrapper functions

initialState = InferrerState { freshName = "a0"
                             , nameStack   = ["(root)"]
                             , records     = defaultRecords
                             , kinds       = defaultKinds
                             , typeClasses = defaultTypeClasses
                             , instances   = defaultInstances }

runInferrer :: Inferrer a -> IO (Either TypeCheckError a, InferrerState)
runInferrer inferrer = flip runStateT initialState $ runErrorT inferrer

runInferrer' :: Inferrer a -> IO (Either TypeCheckError a)
runInferrer' inferrer = do
  (res, env) <- runStateT (runErrorT inferrer) initialState
  return res

typeInference :: M.Map Name Polytype -> Expr -> Inferrer Type
typeInference env e = uncurry applySub <$> infer env e

test :: String -> IO ()
test s = do
  expr <- grab s
  let desugared = desugar expr
  (res, env) <- runInferrer (typeInference mempty desugared)
  case res of
    Left err -> putStrLn $ "Type check error: " ++ err ++ "\n"
    Right t  -> putStrLn $ "Expr: " ++ render 0 expr ++
                           "\nDesugared: " ++ render 0 desugared ++
                           "\nType: " ++ render 0 t ++
                           "\nRecords:  " ++ render 0 (records env) ++
                           "\nTypeClasses: " ++ render 0 (typeClasses env) ++
                           "\nInstances: " ++ render 0 (instances env) ++
                           "\nKinds: " ++ render 0 (kinds env)

-- Main methods

-- | @infer@ is the main type inferrence function
infer :: TypeMap -> Expr -> Inferrer (Substitutions, Type)
infer env expr = do
  case expr of
    String _ -> only str
    Number _ -> only num
    Tuple exprs -> do
      -- subsAndTypes is a list of ({Name: Type}, Type)
      subsAndTypes <- mapM (infer env) exprs
      -- combine all the subs (fst), and make a type tuple of all the types (snd)
      return (mconcat $ map fst subsAndTypes, TTuple $ map snd subsAndTypes)
    Symbol name -> infer env (Var name)
    Var name -> case M.lookup name env of
      -- If it's in the env, we've already type checked it. Instantiate it now
      Just polytype -> only =<< instantiate polytype
      -- If it's not in the env, it might just be declared
      Nothing -> do
        found <- nsLookupRec name
        case found of
          -- If it's declared, we'll trust it but we need to work some magic on it
          Just (Declared typ) -> do
            -- Generalizing will make a polytype with fresh type variables
            let generalized = generalize mempty typ
            -- We then instantiate, which turns it back into a Type
            typ' <- instantiate generalized
            -- and then we can return it with no substitutions
            only typ'
          -- if it's a checked type it should be in the env, but it's ok...
          Just (Checked polytype) -> only =<< instantiate polytype
          -- if it hasn't been declared, that's definitely an error
          Nothing -> inferError $ "Unknown variable: " ++ name
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
      unifyS <- unify (applySub argS funcT) (argT :=> resultT)
                  `catchError` unificationError expr funcT argT
      return (unifyS • argS • funcS, applySub unifyS resultT)
    Let name expr' next -> do
      -- either create a new type variable for the name, or use existing
      newT <- newVarForLet name
      -- create a new environment with that mapping added
      let env1 = M.insert name (bare newT) env
      -- infer expr with that environment
      (exprSubs, exprT) <- pushNS name >> infer env1 expr' >>== popNS
      let exprT' = applySub exprSubs exprT
      subs <- unify newT exprT'
      -- generalize the type with respect to previous env
      let genT = generalize (applySub (subs • exprSubs) env) exprT'
      -- make a record of this type
      register name (Checked genT)
      case next of
        Nothing -> return (exprSubs, tuple [])
        Just next -> do
          -- create a new environment with that generalized type
          let env2 = env ! M.insert name genT ! applySub exprSubs
          -- apply whatever substitutions were produced from evaluating `expr`,
          -- infer the next guy, and compose their substitutions
          (nextSubs, nextT) <- infer env2 next
          return (exprSubs • nextSubs, nextT)
    Sig name typ next -> nsLookup name >>= \case
      Nothing -> register name (Declared typ) >> handle next
      Just _ -> inferError $ "Redeclaration of variable `" ++ name ++ "`"
    TypeClass name types sigs next -> do
      tclass@(TC typ _ sigs) <- makeTypeClass name types sigs
                      `catchError` typeClassError name
      addSigs name typ sigs
      addTypeClass name tclass
      handle next
    Instance tclass typ exprs next -> do
      sigs <- M.fromList <$> forM exprs getNameAndType
      inst <- makeInstance tclass typ sigs `catchError` instanceError tclass typ
      addInstance tclass typ
      handle next
    Datatype typeName vars constructors next -> do
      (kind, funcs) <- makeType typeName vars constructors
                          `catchError` adtError typeName
      forM_ funcs $ \(name, funcT) ->
        register name (Checked $ generalize mempty funcT)
      registerType typeName kind
      handle next
    otherwise -> error $ "FATAL: Unhandleable expression " ++ prettyExpr expr
  where handle next = case next of
          Nothing -> only (tuple [])
          Just expr -> infer env expr
        only t = return (mempty, t)

newvar = newTypeVar []

addTypeClass name _class = modify $
  \s -> s { typeClasses = M.insert name _class (typeClasses s) }

addInstance tclass typ = do
  instanceSet <- gets instances <!> M.lookup tclass >>= \case
    Nothing  -> return $ S.singleton typ
    Just set -> return $ S.insert typ set
  newInstances <- M.insert tclass instanceSet <$> gets instances
  modify (\s -> s { instances = newInstances })

addSigs className (TVar _ tvarName) sigs = do
  -- we need to stick a type class restriction on each occurrence of
  -- the variable in question
  types <- mapM (restrictTC tvarName className) (snd <$> sigs)
  let tRecs = map (Checked . generalize mempty) types
  let sigs' = M.fromList $ zip (fst <$> sigs) tRecs
  newRecords <- M.union sigs' <$> gets records
  modify $ (\s -> s { records = newRecords })

newVarForLet name = nsLookup name >>= \case
  Nothing -> newvar
  Just (Declared t) -> return t
  Just (Checked t) ->
    -- don't allow duplicate definitions
    inferError $ "Redefinition of `" ++ name ++ "`, which had " ++
    "previously been defined in this scope (with type `" ++
    render 0 t ++ "`)"

getNameAndType expr = case expr of
  Let name expr _ -> do
    (subs, typ) <- infer mempty expr
    return (name, applySub subs typ)
  otherwise -> error "FATAL: Non-let expression in instance declaration"

registerType typeName kind = do
  newKinds <- M.insert typeName kind <$> gets kinds
  modify $ \s -> s {kinds = newKinds}

restrictTC tvarName className typ = case typ of
  TVar _ name | name == tvarName -> return $ TVar [className] tvarName
  type1 :=> type2 -> do
    type1' <- rec type1
    type2' <- rec type2
    return $ type1' :=> type2'
  TApply type1 type2 -> do
    type1' <- rec type1
    type2' <- rec type2
    return $ TApply type1' type2'
  TTuple types -> TTuple <$> mapM rec types
  otherwise -> return typ
  where rec = restrictTC tvarName className

-- | Returns the type's kind and the names and types of its constructors
makeType :: Name -> [Name] -> [Constructor] -> Inferrer (Kind, [(Name, Type)])
makeType typeName vars constructors = do
  addType
  checkDups
  -- read each constructor to get its type, and any kind restrictions
  -- on the type variables
  namesSubsTypes <- mapM (readConstructor typeName vars) constructors
  let allSubs = mconcat $ map (\(_, subs, _) -> subs) namesSubsTypes
  let namesTypes = map (\(name, _, typ) -> (name, typ)) namesSubsTypes
  varKinds <- forM vars (findKind allSubs)
  let kind = foldr (:->) Type varKinds
  updateType kind
  return (kind, namesTypes)
  where
    addType = M.member typeName <$> gets kinds >>= \case
      True -> throwError $ "Type `" ++ typeName ++ "` already exists"
      False -> do
        (TVar _ name) <- newTypeVar []
        kinds' <- M.insert typeName (KVar name) <$> gets kinds
        modify $ \s -> s { kinds = M.insert typeName (KVar name) (kinds s) }
    updateType kind =
      modify $ \s -> s { kinds = M.insert typeName kind (kinds s) }
    checkDups = case findDups vars of
      [] -> return ()
      vars -> let present = map (\v -> "`" ++ v ++ "`") ~> intercalate "," in
        throwError $ "Duplicate variable names " ++ present vars
    findDups vars = case vars of
      [] -> []
      v:vs -> (if v `elem` vs then [v] else []) ++ findDups vs
    findKind subs var = do
      case M.lookup var subs of
        Nothing -> return Type
        Just kind -> return kind

readConstructor :: Name                   -- name of the datatype
                -> [Name]                 -- names of bound type variables
                -> Constructor            -- this constructor
                -> Inferrer ( Name        -- constructor name
                            , K.KindSubs  -- inferred variable kinds
                            , Type)       -- type signature of this constructor
readConstructor typeName tvars (Constructor name types) = do
  checkName
  let typ = foldr (:=>) (TConst typeName) types
  let env = M.fromList $ map (\name -> (name, KVar name)) tvars
  (subs, kind) <- K.infer env typ
  unifyS <- K.unify (K.apply subs kind) Type `catchError` singleKindError
  return (name, subs K.• unifyS, typ)
  where
    checkName = nsLookupRec name >>= \case
      Nothing -> return ()
      Just _  -> throwError $ "Type constructor `" ++ name ++ "` already exists"

-- | @generalize@ creates a polytype from a type, by seeing which variables
-- are enclosed by that type and putting them in the variable list
generalize :: TypeMap -> Type -> Polytype
generalize env t =
  let
    -- grab only the variables that are bound in the scope of this type
    vars = S.toList (free t S.\\ free env)
    -- replace them with letters of the alphabet
    vars' = snd <$> zip vars (pure <$> ['a'..])
    -- collect all of the variables' type classes and construct TVars
    types = map (\v -> TVar (getClasses v t) v) vars
    -- this function will make a tuple mapping the old name to the new
    newType (TVar classes name) new = (name, TVar classes [new])
    -- create a substitution map from the old names to the new types
    subs = M.fromList $ zipWith newType types ['a'..]
    -- apply the substitutions to the internal type
    t' = applySub subs t
  -- return a polytype using the new vars and the new type
  in Polytype vars' t'

-- | @instantiate@ is the reverse of generalize; it takes a polytype and
-- creates a normal type by replacing all of the bound variables with
-- fresh variables
instantiate :: Polytype -> Inferrer Type
instantiate (Polytype vars typ) = do
  newVars <- mapM (\var -> newTypeVar (getClasses var typ)) vars
  -- make a substitution mapping all of the variables in the scheme
  -- to new variables we just generated
  let subs = M.fromList (zip vars newVars)
  -- apply those subs to the type and return it
  return $ applySub subs typ

-- | @unify@ creates substitution map which is the set of substitutions
-- sufficient to make the two type arguments equivalent. For example, if
-- we're unifying @a@ and @Number@, we'll have a mapping @a => Number@.
-- If the two types are incompatible, or if there is a cycle in types (for
-- example, mapping @a@ to @b -> a@), an error is thrown.
unify :: Type -> Type -> Inferrer Substitutions
a `unify` b = do
  case (a,b) of
    (TVar classNames u, t) -> do
      res <- t `implements` classNames
      case res of
        True -> u `bind` t
        False -> throwError $
          concat [ "Type class unification error: `", render 0 t
                 , "` does not implement " , case classNames of
                    [name] -> "type class " ++ name
                    names -> "type classes " ++ intercalate ", " names]
    (t, TVar classNames u) -> unify b a
    (TConst n, TConst n') | n == n' -> return mempty
    (TTuple ts1, TTuple ts2) -> mconcat <$> mapM (uncurry unify) (zip ts1 ts2)
    (t1 :=> t2, t3 :=> t4) -> do
      s1 <- t1 `unify` t3
      s2 <- applySub s1 t2 `unify` applySub s1 t4
      return (s1 • s2)
    (TApply l r, TApply l' r') -> unify (l :=> r) (l' :=> r')
    (t1, t2) -> throwError $ "types do not unify: " ++ render 0 t1 ++ " !: " ++
                   render 0 t2
  where
    bind :: Name -> Type -> Inferrer Substitutions
    bind name typ =
      if typ == TVar [] name
      then return mempty
      else if not $ name `S.member` free typ
           then return (M.singleton name typ)
           else throwError $ concat [ "Type cycle detected when attempting "
                                     , "to unify `", render 0 a, "` with `"
                                     , render 0 b, "`: ", name, "` is a free "
                                     , "variable with respect to type `"
                                     , render 0 typ, "`"]

getFull :: Name -> Inferrer Name
getFull name = getNS <!> (++ "." ++ name)

nsLookup :: Name -> Inferrer (Maybe TypeRecord)
nsLookup name = do
  fullname <- getFull name
  gets records <!> M.lookup fullname

nsLookupRec :: Name -> Inferrer (Maybe TypeRecord)
nsLookupRec name = do
  ns <- gets nameStack
  look ns
  where
    look [] = M.lookup name <$> gets records
    look ns = do
      let name' = (name : ns) ! reverse ! intercalate "."
      M.lookup name' <$> gets records >>= \case
        Nothing -> look (tail ns)
        Just typ -> return $ Just typ

register :: Name -> TypeRecord -> Inferrer ()
register name rec = do
  fullname <- getFull name
  modify $ \s -> s { records = M.insert fullname rec (records s) }

pushNS :: Name -> Inferrer ()
pushNS name = modify $ \s -> s {nameStack = name : (nameStack s)}

popNS :: Inferrer ()
popNS = modify $ \s -> s {nameStack = tail (nameStack s)}

getNS :: Inferrer Name
getNS = gets nameStack <!> reverse <!> intercalate "."

-- | @inferPattern@ is a simplified version of @infer@ which works for
-- patterns, which are a subset of expressions (plus the @Placeholder@).
-- any variables encountered are considered new.
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
              `catchError` unificationError pat funcT argT
    return (subs3 • subs2 • subs1, applySub subs3 returnT)
  Placeholder -> only =<< newvar
  Number _    -> only num
  String _    -> only str
  otherwise -> error $ "FATAL: invalid pattern " ++ show pat
  where only t = return (mempty, t)
        newvar = newTypeVar []

prnt :: String -> Inferrer ()
prnt = lift . lift . putStrLn

-- Error handlers
unificationError :: Expr -> Type -> Type -> TypeCheckError -> Inferrer a
unificationError expr funcT argT msg =
  let msg' = concat [ "Type unification error:\n", msg
                    , "\nWhile type checking the expression `"
                    , render 0 expr, "`\nThe left-hand side of "
                    , "this expression has type `", render 0 funcT
                    , "`, and the right-hand side has type `"
                    , render 0 argT, "`."]
  in throwError msg'

typeClassError :: Name -> TypeCheckError -> Inferrer a
typeClassError name msg = let
  msg' = concat [ "Type class error when compiling `", name, "`. Error "
                , "returned was: ", msg]
  in inferError msg'

inferError :: TypeCheckError -> Inferrer a
inferError message = do
  ns <- getNS
  throwError (message ++ "\nOccurred when type checking " ++ ns)

instanceError :: Name -> Type -> TypeCheckError -> Inferrer a
instanceError name typ msg = throwError $
  concat [ "Error attempting to make `", render 0 typ, "` an instance of `"
         , name, "`. Error returned was: ", msg]

inferError' = inferError . concat

adtError typeName msg = inferError' $
  ["Error in datatype declaration `", typeName, "`. Message was: ", msg]

singleKindError msg = throwError $
  "Kind of all constructors must be `Type`, but attempting to unify this " ++
  "failed with message: " ++ msg
