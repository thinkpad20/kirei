{-# LANGUAGE LambdaCase #-}
module TypeClass ( makeTypeClass
                 , makeInstance
                 , isMatch
                 , implements
                 , defaultTypeClasses
                 , defaultKinds
                 , defaultInstances) where

import Common
import Types
import AST
import qualified KindChecker as K
import qualified Data.Map as M
import qualified Data.Set as S

-- | @makeTypeClass@ takes the parsed information of a TypeClass and constucts
-- a proper one; it checks for errors like name overlap, and infers the kinds
-- of the parameterized types.
makeTypeClass :: Name -> [Type] -> [Expr] -> Inferrer TypeClass
makeTypeClass name types parsedSigs = case types of
  [typ@(TVar classes varName)] -> do
    checkUnique typeClasses
    checkUnique kinds
    checkUnique records
    sigs <- forM parsedSigs $ \case
      Sig name typ _ -> return (name, typ)
      someExpr -> error $ "FATAL: Invalid expression `" ++
                          show someExpr ++ "` in type class signature list"
    checkDups sigs
    kind <- inferKindFromSigs classes varName sigs
    return $ TC typ kind sigs
  types ->
    throwError "Multi-parameter type classes are not yet supported"
  where
    checkUnique map = do
      gets map <!> M.lookup name >>= \case
        Just _ -> throwError $ "Name of type class `" ++ name ++ "` in use"
        Nothing -> return ()
    checkDups sigs = case hasDups (map fst sigs) of
      [] -> return ()
      names -> let present = map show ~> intercalate "," in
        throwError $ "Duplicate method names " ++ present names
    -- This isn't an efficient implementation but it should do the job
    hasDups nameList = case nameList of
      [] -> []
      n:ns -> (if n `elem` ns then [n] else []) ++ hasDups ns

inferKindFromSigs :: [Name]         -- list of parent classes
                  -> Name           -- name of bound variable
                  -> [(Name, Type)] -- names and types of methods
                  -> Inferrer Kind  -- inferred kind of the class
inferKindFromSigs classes varName sigs = do
  -- make a new variable (will use in a kind)
  (TVar _ kname) <- newTypeVar []
  -- make a singleton map from varName to that kind
  let env = M.singleton varName (KVar kname)
  inferredKinds <- forM sigs $ \(name, typ) -> do
    -- infer the kind of this type
    (subs, kind) <- K.infer env typ
    -- it must be kind `Type`
    subs2 <- K.unify (K.apply subs kind) Type
    let totalSubs = (subs K.• subs2)
    -- and its subs must mention the type variable we're inspecting
    case M.lookup kname totalSubs of
      Nothing -> throwError $ "Class method `" ++ name ++ "` does not " ++
                              "mention class type variable `" ++ varName ++ "`"
      Just tcKind -> return tcKind
  kind <- check inferredKinds
  forM_ classes (K.matchWithParent kind)
  return kind
  where check [] = error $ "FATAL: no signatures in type class"
        check (k:ks) =
          if all (== k) ks then return k
          else throwError $ "Inferred multiple kinds for variable `" ++
                            varName ++ "` based on usage in method signatures"

-- | @lookupKind@ checks our type dictionary and finds the kind of a
-- type constant
lookupKind :: Name -> Inferrer Kind
lookupKind name = gets kinds <!> M.lookup name >>= \case
  Just kind -> return kind
  Nothing -> throwError $ "Unknown type constant `" ++ name ++ "`"

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
    getParents className = gets typeClasses <!> M.lookup className >>= \case
      Nothing -> error $ "FATAL: type class `" ++ className ++ "` wasn't found"
      Just tclass -> return $ inherits tclass

-- | @isInstance@ checks if a list of types make up an instance of a type
-- class. It looks the set of instances up in the @instances@ dictionary and
-- iterates through it, asking if the types are a match for any of the instances.
isInstance :: Type -> Name -> Inferrer Bool
typ `isInstance` className = do
  --prnt $ "Seeing if " ++ render 0 typ ++ " is an instance of " ++ className
  gets instances <!> M.lookup className >>= \case
    Nothing -> return False
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

-- | Returns an instance if valid. Checks that:
--    1) the class exists
--    2) the kind of the type provided is the same as that class
--    3) an instance doesn't already exist for that type
--    4) the instance method signatures match
makeInstance :: Name -> Type -> M.Map Name Type -> Inferrer Instance
makeInstance className typ funcs = do
  -- make sure class exists
  (TC _ kind sigs) <- M.lookup className <$> gets typeClasses >>= \case
    Nothing -> throwError $ "Type class `" ++ className ++ "` does not exist"
    Just tclass -> return tclass
  -- get kind of type
  thisKind <- K.infer' mempty typ
  -- make sure equal
  if kind /= thisKind
  then throwError' [ "Kind mismatch: `", className, "` expects kind `"
                   , render 0 kind, "` but type `"
                   , render 0 typ, "` has kind `", render 0 thisKind, "`"]
  -- check if already implements
  else typ `implements` [className] >>= \case
    -- already has an instance
    True -> throwError' [ "An instance for `", className, "` already "
                        , "exists for `", render 0 typ, "`"]
    -- new instance
    False -> do
      -- need to check if method sigs match
      forM_ sigs checkSig
      -- if so we can return
      return typ
  where
    checkSig (name, typ') = case M.lookup name funcs of
      Nothing -> throwError' ["Method `", name, "` is not implemented"]
      Just typ -> typ `isMatch` typ' >>= \case
        False -> throwError' [ "Type mismatch in method `", name
                             , "`: method should have type `"
                             , render 0 typ', "` but instead it has "
                             , "type `", render 0 typ, "`"]
        True -> return ()

throwError' = throwError . concat

defaultKinds = M.fromList
  [
    ("String", Type)
  , ("Number", Type)
  , ("Bool", Type)
  , ("[]", Type :-> Type)
  ]

defaultTypeClasses :: M.Map Name TypeClass
defaultTypeClasses = M.fromList
  [
    ("Applicative", TC (TVar ["Functor"] "f") (Type :-> Type) [pure, apply])
  , ("Functor", TC (TVar [] "f") (Type :-> Type) [map])
  , ("Monad", TC (TVar ["Applicative"] "m") (Type :-> Type) [return, bind])
  , ("Show", TC (TVar [] "a") Type [show])
  , ("Eq", TC (TVar [] "a") Type [eq])
  , ("Ord", TC (TVar ["Eq"] "a") Type [succ])
  ]
  where a = TVar [] "a"
        a' name = TVar [name] "a"
        b = TVar [] "b"
        f = TApply (TVar [] "f")
        m = TApply (TVar [] "m")
        pure = sig "pure" (a :=> f a)
        apply = sig "<*>" (f (a :=> b) :=> f a :=> f b)
        map = sig "map" ((a :=> b) :=> f a :=> f b)
        eq = sig "==" (a :=> a :=> bool)
        succ = sig "succ" (a :=> a)
        return = sig "return" (a :=> m a)
        bind = sig ">>=" (m a :=> (a :=> m b) :=> m b)
        show = sig "show" (a :=> str)

defaultInstances :: M.Map Name (S.Set Instance)
defaultInstances = M.fromList
  [
    ("Num",         S.fromList [num])
  , ("Eq",          S.fromList [ num
                               , str
                               , bool
                               , listOf (a ["Eq"])
                               ])
  , ("Show",        S.fromList [ num
                               , str
                               , bool
                               , listOf (a ["Show"])
                               ])
  , ("Monoid",      S.fromList [ num
                               , listOf (a [])])
  , ("Ord",         S.fromList [ num
                               , str
                               , listOf (a ["Ord"])])
  , ("Applicative", S.fromList [list])
  , ("Functor",     S.fromList [list])
  ]
  where list = TConst "[]"
        listOf = TApply list
        a classes = TVar classes "a"
