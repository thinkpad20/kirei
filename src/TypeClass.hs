{-# LANGUAGE LambdaCase #-}
module TypeClass where

import Common
import Types
import AST
import qualified Data.Map as M
import qualified Data.Set as S

-- | @makeTypeClass@ takes the parsed information of a TypeClass and constucts
-- a proper one; it checks for errors like name overlap, and infers the kinds
-- of the parameterized types.
makeTypeClass :: Name -> [Type] -> [Expr] -> Inferrer TypeClass
makeTypeClass name types parsedSigs = case types of
  [typ] -> do
    checkUnique name
    sigs <- flip mapM parsedSigs $ \case
      Sig name typ _ -> return (name, typ)
      e -> throwError $ "FATAL: Invalid expression `" ++ show e ++
                        " `in type class signature list"
    checkDups sigs
    kind <- inferKind typ sigs
    mapM_ validate sigs
    return $ TC typ kind sigs
  types ->
    throwError "Multi-parameter type classes are not yet supported"
  where
    checkUnique name = do
      get <!> typeClasses <!> M.lookup name >>= \case
        Just _ -> throwError $ "Redeclaration of type class `" ++ name ++ "`"
        Nothing -> return ()
    checkDups sigs = case hasDups (map fst sigs) of
      [] -> return ()
      names -> let present = map show ~> intercalate "," in
        throwError $ "Duplicate method names " ++ present names
    -- This isn't an efficient implementation but it should do the job
    hasDups nameList = case nameList of
      [] -> []
      n:ns -> (if n `elem` ns then [n] else []) ++ hasDups ns


-- | @inferKind@ takes a type variable parameterizing a type class,
-- looks through all of its type signatures and finds the kind
-- of the type based on them.
inferKind :: Type -> [(Name, Type)] -> Inferrer Kind
inferKind (TVar classes name) sigs = checkKinds =<< mapM find sigs where
  find :: (Name, Type) -> Inferrer Kind
  find (funcName, typ) = check funcName $ f typ where
    f :: Type -> Either String [Kind]
    f typ = case typ of
      -- if it's this variable, return a single type
      TVar _ name' | name == name' -> return [type_]
      TApply tL tR -> f tL >>= \kL -> f tR >>= \kR -> case (kL, kR) of
        -- no kinds found here (e.g. the @(a->b)@ in fmap)
        ([], []) -> return []
        -- kind found on right hand side of application
        ([], kind : _) -> return [kind]
        -- kind found on left-hand side of application (e.g. @f a@ in fmap)
        (kind : _, []) -> return [kind :=> type_]
        -- error, e.g. if we tried to make a signature @f f@
        (kinds, kinds') -> Left $ "Infinite kind, type variable `" ++ name ++
                                  "` is applied to itself"
      -- in a function just append the two results together
      t1 :=> t2 -> pure (++) <*> f t1 <*> f t2
      -- just map over the types
      TTuple types -> concat <$> mapM f types
      -- other cases are constant types or other type variables
      otherwise -> return []

  -- | @check@ will wrap errors found by @find@, will ensure at least one kind
  -- was inferred, and make sure that all kinds inferred are equal
  check :: Name -> Either String [Kind] -> Inferrer Kind
  check fname kinds = case kinds of
    Left err -> throwError err
    Right [] -> throwError $ "Type variable `" ++ name ++ "` was not " ++
                       "found in method `" ++ fname ++ "`, could not infer kind"
    Right (k:kinds) ->
      if all (== k) kinds then return k
      else throwError $ "Inferred multiple kinds for `" ++ name ++ "`"

  -- | @checkKinds@ makes sure that all kinds inferred are equal and if
  -- parent classes have been declared, that it's equal to them too.
  checkKinds :: [Kind] -> Inferrer Kind
  checkKinds [] = error $ "FATAL: no kinds generated"
  checkKinds (k:ks) =
    if not $ all (== k) ks then throwError $ "Non-matching kinds for type " ++
                                           " variable `" ++ name ++ "`"
    else do
      -- find the kinds of its parents (will also check existence)
      parentKinds <- mapM lookupKindFromTypeClass classes
      -- make sure they all have the same kind
      if all (== k) parentKinds then return k
      else throwError $ "Kind of `" ++ name ++ "` does not match kind " ++
                            "of one or more parent classes"


defaultTypeClasses :: M.Map Name TypeClass
defaultTypeClasses = M.fromList
  [
    ("Applicative", TC (TVar ["Functor"] "g") (type_ :=> type_) [pure_, apply_])
  , ("Functor", TC (TVar [] "f") (type_ :=> type_) [map_])
  , ("Monad", TC (TVar ["Applicative"] "m") (type_ :=> type_) [return_, bind_])
  , ("Show", TC (TVar [] "a") type_ [show_])
  , ("Eq", TC (TVar [] "a") type_ [eq_])
  , ("Ord", TC (TVar ["Eq"] "a") type_ [succ_])
  ]
  where a = TVar [] "a"
        a' name = TVar [name] "a"
        b = TVar [] "b"
        f = TApply (TVar ["Functor"] "f")
        g = TApply (TVar ["Applicative"] "g")
        m = TApply (TVar ["Monad"] "m")
        pure_ = sig "pure" (a :=> g a)
        apply_ = sig "<*>" (g (a :=> b) :=> g a :=> g b)
        map_ = sig "map" ((a :=> b) :=> f a :=> f b)
        eq_ = sig "==" (a' "Eq" :=> a' "Eq" :=> bool)
        succ_ = sig "succ" (a' "Ord" :=> a' "Ord")
        return_ = sig "return" (a :=> m a)
        bind_ = sig ">>=" (m a :=> (a :=> m b) :=> m b)
        show_ = sig "show" (a' "Show" :=> str)

lookupKindFromTypeClass :: Name -> Inferrer Kind
lookupKindFromTypeClass name = gets typeClasses <!> M.lookup name >>= \case
  Just (TC _ kind _) -> return kind
  Nothing -> throwError $ "Parent class `" ++ name ++ "` does not exist"

-- | @lookupKind@ checks our type dictionary and finds the kind of a
-- type constant
lookupKind :: Name -> Inferrer Kind
lookupKind name = gets kinds <!> M.lookup name >>= \case
  Just kind -> return kind
  Nothing -> throwError $ "Unknown type constant `" ++ name ++ "`"

-- | @validate@ will ensure that a type has a kind `Type`
validate :: Sig -> Inferrer ()
validate (name, typ) = do
  kind <- checkKind typ
          `catchError`
          \msg ->
          throwError $ msg ++ "\nWhile checking function `" ++ name ++ "`"
  if isSingleKind kind then return ()
  else throwError $ "Function `" ++ name ++ "` must have a " ++
                                   "single kind, but was found with the " ++
                                   "kind `" ++ render 0 kind ++ "`"


-- | @checkKind@ is similar to inferKind but can't make any actual inferrence.
-- it just finds the kind or throws an error, for example if it encounters
-- a single-kind type applied to another type (like `String a`).
checkKind :: Type -> Inferrer Kind
checkKind typ = case typ of
  TConst name -> lookupKind name
  TVar classes name -> mapM lookupKindFromTypeClass classes >>= \case
    [] -> return type_
    k:kinds -> if all (== k) kinds then return k
               else throwError $ "Mismatching kinds in parent classes of "
                                 ++ "type variable `" ++ name ++ "`"
  TApply left right -> do
    leftKind <- checkKind left
    case leftKind of
      paramK :=> resultK -> do
        argK <- checkKind right
        if argK == paramK then return resultK
        else throwError kindMismatchError
      singleKind -> throwError kindMismatchError
      -- problem here, if we say `a b` and `a` is not given a type class,
      -- we'll throw an error. Really, we should be doing a HM-style
      -- inferrence here, but we're more rudimentary. It's OK for now.
      where kindMismatchError = concat [ "Type `", render 0 left
                                       , "` is applied to another type `"
                                       , render 0 right, "` but its kind is "
                                       , "either unknown or is a single `Type`"]
  TTuple types -> do
    kinds <- mapM checkKind types
    if all isSingleKind kinds then return type_
    else throwError $ "All types in a type tuple must be single kinds, " ++
                      "test failed in type tuple " ++ render 0 typ
  t1 :=> t2 -> do
    k1 <- checkKind t1
    k2 <- checkKind t2
    if isSingleKind k1 && isSingleKind k2 then return type_
    else throwError $ "Functions must map from single kinds to single kinds, " ++
                      "but type " ++ render 0 typ ++ " does not"

isSingleKind :: Type -> Bool
isSingleKind typ = typ == type_

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
    otherwise -> typ `isInstance` className

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


defaultKinds = M.fromList
  [
    ("String", type_)
  , ("Number", type_)
  , ("Bool", type_)
  , ("[]", type_ :=> type_)
  ]
