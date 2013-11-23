module TypeChecker where

import Parser
import Data.List (intercalate)
import Control.Applicative ((<$>))
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe (fromJust)
import Debug.Trace

data Type =
  NumberType
  | StringType
  | TypeVar Name
  | TupleType [Type]
  | NamedTuple Name [Type]
  | Type :=> Type
  | UnionType [Type]
  deriving (Eq, Ord)

instance Show Type where
  show t = case t of
    NumberType -> "Number"
    StringType -> "String"
    TypeVar name -> name
    TupleType ts -> concat ["(", intercalate ", " $ show <$> ts, ")"]
    NamedTuple name ts -> name ++ case ts of
      [] -> ""
      [t] -> " " ++ show' t
      ts -> "(" ++ intercalate " " (show' <$> ts) ++ ")"
    t1 :=> t2 -> case t1 of
      _ :=> _ -> concat ["(", show t1, ")", " -> ", show t2]
      _ -> show t1 ++ " -> " ++ show t2
    where show' NumberType = show NumberType
          show' StringType = show StringType
          show' (TypeVar n) = n
          show' (NamedTuple n []) = show n
          show' t@(TupleType _) = show t
          show' t = "(" ++ show t ++ ")"

type TypeMap = M.Map Name Type
type UsedTypeNames = S.Set Name
type TypeMaps = [TypeMap]
type Env = (TypeMaps, UsedTypeNames)

num = NumberType
str = StringType
infixr 3 :=>
list a = NamedTuple "List" [a]
array a = NamedTuple "Array" [a]
bool = NamedTuple "Bool" []

start :: TypeMaps
start =
  [
    M.fromList [
      ("+", num :=> num :=> num),
      ("-", num :=> num :=> num),
      ("*", num :=> num :=> num),
      ("/", num :=> num :=> num),
      ("at", str :=> num :=> str),
      ("True", bool),
      ("False", bool),
      ("[]", list $ TypeVar "a"),
      ("::", TypeVar "a" :=> (list $ TypeVar "a")),
      ("length", list $ TypeVar "a")
    ]
  ]



pushTM :: TypeMaps -> TypeMaps
pushTM m = M.empty : m

popTM :: TypeMaps -> (TypeMap, TypeMaps)
popTM (m:ms) = (m, ms)

lookupType :: Env -> String -> Maybe Type
lookupType (tmaps, _) name = look tmaps name where
  look [] _ = Nothing
  look (m:ms) name = case M.lookup name m of
    Nothing -> look ms name
    Just t -> Just t

addType :: Env -> String -> Type -> Env
addType ((m:ms), u) name t = (
                                (M.insert name t m:ms),
                                S.insert (show t) u
                              )

getNewTypeName :: Env -> (Type, Env)
getNewTypeName (tmaps, used) = give "a" where
  give name = case name `S.member` used of
    True -> give $ next name
    False -> (TypeVar name, (tmaps, S.insert name used))
  next name = let (c:cs) = reverse name in
    reverse (if c < 'z' then succ c : cs else 'a' : c : cs)

mismatchError :: Type -> Type -> a
mismatchError argT funcT = error $
  "Error: couldn't match " ++ show argT ++ " with " ++ show funcT

applyError :: Type -> Type -> a
applyError argT funcT = error $ concat
  ["Error: ", show funcT, " is not a function (applied to ", show argT, ")"]

update env@(tmaps, used) name typ = (newTmaps, newUsed) where
  -- we need to update the association: any variable currently mapped
  -- to `name` should now be mapped to typ.
  replaceType t = case t of
    TypeVar n | n == name -> typ
    t1 :=> t2 -> replaceType t1 :=> replaceType t2
    TupleType ts -> TupleType (replaceType <$> ts)
    NamedTuple n ts -> NamedTuple n (replaceType <$> ts)
    UnionType ts -> UnionType (replaceType <$> ts)
    _ -> t
  newTmaps = fmap (fmap replaceType) tmaps
  newUsed = name `S.delete` used

infer :: Env -> Expr -> (Type, Env)
infer env@(tmaps, used) expr = case expr of
  Number _ -> (NumberType, env)
  String _ -> (StringType, env)
  Symbol s -> infer env (Var s)
  Var v -> case lookupType env v of
    Just t -> (t, env)
    Nothing -> let (t, env') = getNewTypeName env in
      (t, (addType env' v t))
  Apply e1 e2 ->
    let (argT, envAfterArg) = infer env e2
        (funcT, envAfterFunc) = infer envAfterArg e1 in
    -- try to look up type of the variable being applied
    case funcT of
      -- if it's just some random variable, make that variable
      -- a function from the argument type to a new type variable
      TypeVar v -> (newT, newEnv) where
        (newT, envAfterNewName) = getNewTypeName envAfterFunc
        newEnv = update envAfterNewName v (argT :=> newT)
      -- if it's a function, then ensure its parameter type is right
      paramT :=> newT -> (newT, newEnv) where
        newEnv = case argT of
          -- if the argument is a type variable `v`, we can update `v`
          -- to be whatever func takes as its parameter, and return a `newT`
          TypeVar v -> update envAfterFunc v paramT
          -- otherwise, it had better be the argument we're expecting
          t | t == paramT -> envAfterFunc
            | otherwise -> mismatchError t paramT
      _ -> error $ show funcT ++ " is not a function"

--startInfer = infer (start, S.empty)

startInfer expr = (runStateT . infer2) expr (start, S.empty)

test = runIdentity . startInfer . grab

testInputs =
  [
    ("a", TypeVar "a"),
    ("a *", NumberType :=> NumberType),
    ("a * b", NumberType),
    ("\"hello\"", StringType),
    ("map (2 *) foo", TypeVar "b")
  ]

runTests = mapM_ run testInputs where
  run (input, result) = do
    let res = fst $ test input
    if res /= result
      then putStrLn $ c ["Failed test! (", input, ") !: ", show res]
      else putStrLn $ c ["Test passed! (", input, ") : ", show result]
  c = concat


type Inferrer a = StateT Env Identity a

updateEnv :: Name -> Type -> Inferrer Type
updateEnv name typ = do
  (tmaps, used) <- get
  put (newTmaps tmaps, newUsed used)
  return typ
  where
    -- we need to update the association: any variable currently mapped
    -- to `name` should now be mapped to typ.
    replaceType t = case t of
      TypeVar n | n == name -> typ
      t1 :=> t2 -> replaceType t1 :=> replaceType t2
      TupleType ts -> TupleType (replaceType <$> ts)
      NamedTuple n ts -> NamedTuple n (replaceType <$> ts)
      UnionType ts -> UnionType (replaceType <$> ts)
      _ -> t
    newTmaps = fmap (fmap replaceType)
    newUsed = S.delete name

--(=>=) :: Type -> Type -> Inferrer Type
--TypeVar a =>= t = updateEnv a t
--(t1 :=> t2) =>= (t3 :=> t4) = do
--  t1' <- t1 =>= t3
--  t2' <- t2 =>= t4
--  t1' =>= t2'
--TupleType as =>= TupleType bs = undefined
--  -- this won't type match, need to thinking about it
--  return $ TupleType $ uncurry (=>=) $ zip as bs
--NamedTuple n as =>= NamedTuple m bs | n == m =
--  return $ NamedTuple n $ uncurry (=>=) $ zip as bs

getNewTypeName2 :: Inferrer Type
getNewTypeName2 = give "a" where
  give name = do
    (tmaps, used) <- get
    case name `S.member` used of
      True -> give $ next name
      False -> do
        put (tmaps, S.insert name used)
        return $ TypeVar name
  next name = let (c:cs) = reverse name in
    reverse (if c < 'z' then succ c : cs else 'a' : c : cs)

lookupType2 :: String -> Inferrer (Maybe Type)
lookupType2 name = do
  (tmaps, _) <- get
  look tmaps name where
    look [] _ = return Nothing
    look (m:ms) name = case M.lookup name m of
      Nothing -> look ms name
      Just t -> return $ Just t

addType2 :: Name -> Type -> Inferrer Type
addType2 name t = do
  ((m:ms), u) <- get
  put (M.insert name t m:ms, S.insert (show t) u)
  return t

infer2 :: Expr -> Inferrer Type
infer2 expr = do
  env@(tmaps, used) <- get
  case expr of
    Number _ -> return NumberType
    String _ -> return StringType
    Symbol s -> infer2 (Var s)
    Var v -> do
      tp <- lookupType2 v
      case tp of
        Just t -> return t
        Nothing -> do
          t <- getNewTypeName2
          addType2 v t
          return t
    Apply e1 e2 -> do
      argT <- infer2 e2
      funcT <- infer2 e1
      -- try to look up type of the variable being applied
      case funcT of
        -- if it's just some random variable, make that variable
        -- a function from the argument type to a new type variable
        TypeVar v -> do
          env' <- get
          newT <- getNewTypeName2
          updateEnv v (argT :=> newT)
        -- if it's a function, then ensure its parameter type is right
        paramT :=> newT ->
          case argT of
            -- if the argument is a type variable `v`, we can update `v`
            -- to be whatever func takes as its parameter, and return a `newT`
            TypeVar v -> do
              updateEnv v paramT
              return newT
            -- otherwise, it had better be the argument we're expecting
            t | t == paramT -> return newT
              | otherwise -> mismatchError t paramT
        _ -> error $ show funcT ++ " is not a function"
