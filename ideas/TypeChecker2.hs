module TypeChecker where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (intercalate)
import Control.Applicative ((<$>))
import Control.Monad.State
import Prelude hiding (lookup)
import Parser

data Type =
  NumberType
  | StringType
  | TypeVar Name
  | TupleType [Type]
  | NamedTuple Name [Type]
  | Type :=> Type
  | UnionType [Type]
  | Polytype Name Type
  deriving (Eq, Ord)

type TypeMap = M.Map Name Type
type Used = S.Set Name
type Env = (TypeMap, Used)
type Inferrer = StateT [Env] IO

initEnv = (M.empty, S.empty)

pushEnv :: Inferrer ()
pushEnv = modify (\envs -> initEnv : envs)

popEnv :: Inferrer Env
popEnv = do
  (env:envs) <- get
  put envs
  return env

lookup :: Name -> Inferrer (Maybe Type)
lookup name = do
  ((tmap, _):_) <- get
  return $ M.lookup name tmap

store :: Name -> Type -> Inferrer Type
store name typ = modify foo >> return typ where
  foo (tmaps, used):env = (M.insert name typ tmaps, used):env

-- Returns a shiny new unused type variable
newVar :: Inferrer Type
newVar = do
  (tmaps, used):envs <- get
  let name = give "a" used
  put ((tmaps, S.insert name used):envs)
  return $ TypeVar name
  where
    give n u = if n `S.member` u then give (next n) u else n
    next name = let (c:cs) = reverse name in
      reverse (if c < 'z' then succ c : cs else 'a' : c : cs)

--store :: Name -> Polytype ->

{-

inferring a Let x a b

Inside of a, it needs to have access to all outer-scope type
variables, but it should be able to override them. For example
if this is (\x -> x +), then we'll need to know that +
is of type Num -> Num -> Num, but the temporary assignment for
x can be anything, and we'll look at it from the top down so
name conflicts only matter in the current scope.

ex: let id = \x -> x; (id 1, id "hello");

we determine id : All a.a -> a
this is a polytype
we're applying id to id
we look up id and see a polytype (All a.a -> a)
we need to instantiate that into a monotype, just for now (not store it)
we choose a1 -> a1, return that
now

OK we're evaluating a lambda: \f -> f 2

We push a new typemap onto the stack, with f being some unknown
variable a:

push {f: a}

Then we go and evaluate the expression: Apply f 2

we evaluate the argument, and get a number. But we don't know what
f 2 returns. So we need to get a new type variable for that, b.

Then we say OK, we need f to be a function that takes a number and
returns a b. So we need to unify `a` and `num -> b`.

unify a (num -> b)

This unification will examine f, see that it's a variable `a`, and make
a substitution of all `a`s in the environment to num -> bs. This update
is only within the scope of f, not above it (e.g. there could be some earlier
type `a` instantiated somewhere, and we don't want to set that)


What if the input was \f -> \g -> f (g 2)?

We push f on the stack with `a`, and g with `b`:
[{g: b}, {f: a}]
we determine g is `num -> c` by the above process
then we make a new type variable `d` which this returns, and
say that `f : c -> d`

-}
mkSubs :: TypeMap -> Type -> Type
mkSubs tmap t = case t of
  NumberType ->
  StringType ->
  TypeVar name ->
  TupleType ts ->
  NamedTuple name ts ->
  t1 :=> t2 ->
  Polytype var t ->


infer :: Expr -> Inferrer Type
infer expr = case expr of
  Number _ -> return NumberType
  String _ -> return StringType
  Symbol s -> infer (Var s)
  Var name -> do
    maybeType <- lookup name
    case maybeType of
      Nothing -> error $ "Undefined variable '" ++ name ++ "'"
      Just (Polytype vars
       typ) -> do
        -- get a new variable for each of these
        newVars <- mapM newVar vars
        -- make all of the substitutions in the type
        return $ mkSubs (M.fromList $ zip vars newVars)
      Just t -> return t


startInfer expr = (runStateT . infer) expr (start, S.empty)

start :: TypeMap
start =
  M.fromList [
    ("+", MonoType $ NumberType :=> NumberType :=> NumberType)
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

infixr 3 :=>
tvar = TypeVar
list a = NamedTuple "List" [a]
list' = list . tvar
array a = NamedTuple "Array" [a]
bool = NamedTuple "Bool" []

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
    Polytype var t -> "∀" ++ var ++ "." ++ show t
    where show' NumberType = show NumberType
          show' StringType = show StringType
          show' (TypeVar n) = n
          show' (NamedTuple n []) = show n
          show' t@(TupleType _) = show t
          show' t = "(" ++ show t ++ ")"
