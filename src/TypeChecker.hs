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
import Prelude hiding (lookup)
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

type TypeMap = M.Map Name Type
type UsedTypeNames = S.Set Name
type TypeMaps = [TypeMap]
type Env = (TypeMaps, UsedTypeNames)

pushI :: Inferrer ()
pushI = do
  (tmaps, used) <- get
  put (M.empty : tmaps, used)

popI :: Inferrer TypeMap
popI = do
  ((m:ms), used) <- get
  put (ms, used)
  return m

mismatchError :: Type -> Type -> a
mismatchError argT funcT = error $
  "Error: couldn't match " ++ show argT ++ " with " ++ show funcT

applyError :: Type -> Type -> a
applyError argT funcT = error $ concat
  ["Error: ", show funcT, " is not a function (applied to ", show argT, ")"]

startInfer expr = (runStateT . infer) expr (start, S.empty)

type Inferrer a = StateT Env IO a

getNewTypeName :: Inferrer Type
getNewTypeName = give "a" where
  give name = do
    (tmaps, used) <- get
    case name `S.member` used of
      True -> give $ next name
      False -> do
        --lift $ putStrLn $ "Found a new name '" ++ name ++ "'"
        put (tmaps, S.insert name used)
        return $ TypeVar name
  next name = let (c:cs) = reverse name in
    reverse (if c < 'z' then succ c : cs else 'a' : c : cs)

lookup :: String -> Inferrer (Maybe Type)
lookup name = do
  (tmaps, _) <- get
  look tmaps name where
    look [] _ = return Nothing
    look (m:ms) name = case M.lookup name m of
      Nothing -> look ms name
      Just t -> return $ Just t

addType :: Name -> Type -> Inferrer Type
addType name t = do
  modEnv (\(m:ms) -> M.insert name t m:ms) (S.insert (show t))
  return t

--p' = lift . putStrLn

-- | Will seek to "unify" types t1 and t2, meaning that any free
-- variables in t1 will be assigned to corresponding ones in t2.
-- If there is a type mismatch, we'll fail.
unify :: Type -> Type -> Inferrer ()
unify origT newT = case (origT, newT) of
  (_, TypeVar _) -> unify newT origT -- order is irrelevant
  (TypeVar name, newT) -> updateMaps name newT >> updateUsed name
  (a :=> b, c :=> d) -> unify a c >> unify b d
  (t1, t2) -> error $ "Can't deal with " ++ show t1 ++ ", " ++ show t2
  where
    replaceType name newT origT = case origT of
      TypeVar n | n == name -> newT
      t :=> t' -> replaceType name newT t :=> replaceType name newT t'
      TupleType ts -> TupleType (replaceType name newT <$> ts)
      NamedTuple n ts -> NamedTuple n (replaceType name newT <$> ts)
      UnionType ts -> UnionType (replaceType name newT <$> ts)
      _ -> origT
    updateMaps name typ = do
      addType name typ
      modTmaps $ fmap (fmap $ replaceType name typ)
    updateUsed name = modUsed $ S.delete name



infer :: Expr -> Inferrer Type
infer expr = case expr of
  Number _ -> return NumberType
  String _ -> return StringType
  Symbol s -> infer (Var s)
  Var v -> do
    tp <- lookup v
    case tp of
      Just t -> return t
      Nothing -> getNewTypeName >>= addType v
  Apply e1 e2 -> do
    argT <- infer e2 -- type of the argument e2
    funcT <- infer e1 -- type of the function e1, if it is one
    r@(TypeVar retT) <- getNewTypeName -- type that this expr will evaluate to
    unify funcT (argT :=> r) -- this will fail if types mismatch
    -- if we can look up retT it means it's been assigned to something
    maybeReturnT <- lookup retT
    return $ case maybeReturnT of
      Nothing -> r
      Just returnT -> returnT
  Lambda n e -> do
    argT <- (getNewTypeName >>= addType n)
    returnT <- infer e
    return $ argT :=> returnT


test = startInfer . grab

testInputs =
  [
    ("a", TypeVar "a"),
    ("a *", NumberType :=> NumberType),
    ("a * b", NumberType),
    ("\"hello\"", StringType),
    ("map (2 *) foo", TypeVar "b")
  ]

verify :: Type -> Type -> Bool
verify (TypeVar _) (TypeVar _) = True
verify (a :=> b) (c :=> d) = verify a c && verify b d
verify (TupleType as) (TupleType bs) = and $ zipWith verify as bs
verify (NamedTuple n as) (NamedTuple m bs) =
  n == m && (and $ zipWith verify as bs)
verify t1 t2 = t1 == t2

runTests = mapM_ run testInputs where
  run (input, result) = do
    (res, _) <- test input
    if not $ verify res result
      then putStrLn $ c ["Failed test! (", input, ") !: ", show res]
      else putStrLn $ c ["Test passed! (", input, ") : ", show result]
  c = concat

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

modEnv f g = do
  (tmaps, used) <- get
  put (f tmaps, g used)

modTmaps :: (TypeMaps -> TypeMaps) -> Inferrer ()
modTmaps f = modEnv f id

modUsed :: (UsedTypeNames -> UsedTypeNames) -> Inferrer ()
modUsed f = modEnv id f

num = NumberType
str = StringType
infixr 3 :=>
tvar = TypeVar
list a = NamedTuple "List" [a]
list' = list . tvar
array a = NamedTuple "Array" [a]
bool = NamedTuple "Bool" []

start :: TypeMaps
start =
  [
    M.fromList [
      ("+", num :=> num :=> num)
      --("map", (tvar "a" :=> tvar "b") :=> list' "a" :=> list' "b")
      --("-", num :=> num :=> num),
      --("*", num :=> num :=> num),
      --("/", num :=> num :=> num),
      --("at", str :=> num :=> str),
      --("True", bool),
      --("False", bool),
      --("[]", list $ TypeVar "a"),
      --("::", TypeVar "a" :=> (list $ TypeVar "a")),
      --("length", list $ TypeVar "a")
    ]
  ]
