module TypeChecker where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Common
import AST
import Prelude hiding (lookup)
import Parser

data Type =
  TypeVar Name
  | NamedType Name [Type]
  | Type :=> Type
  deriving (Eq, Ord)

infixr 4 :=>

instance Show Type where
  show t = case t of
    TypeVar name -> name
    -- a named type with no name is just a tuple
    NamedType "" ts -> "(" ++ (intercalate ", " $ map show ts) ++ ")"
    NamedType name [] -> name
    NamedType name ts -> name ++ " " ++ (intercalate " " $ map show' ts)
    t1 :=> t2 -> show' t1 ++ " -> " ++ show t2
    where show' t@(NamedType _ (_:_)) = "(" ++ show t ++ ")"
          show' t@(a :=> b) = "(" ++ show t ++ ")"
          show' t = show t

num, str, bool :: Type
num  = NamedType "Number" []
str  = NamedType "String" []
bool = NamedType "Bool"   []

newtype TypeMap = TypeMap (M.Map Name Type)
type UsedNames = S.Set Name
type Env = [(TypeMap, UsedNames)]
type Inferrer = StateT Env IO

instance Show TypeMap where
  show (TypeMap env) = let
    pairs = M.toList env
    toS (key, val) = show key ++ ": " ++ show val
    in "{" ++ (intercalate ", " $ map toS pairs) ++ "}"

getVar :: Name -> Inferrer Type
getVar name = do
  env@((tmap, used):es) <- get
  lookupRec env where
    lookupRec [] = do
      var <- newVar
      modTmap (\(TypeMap m) -> (TypeMap $ M.insert name var m))
      return var
    lookupRec ((TypeMap tmap, _):env) = case M.lookup name tmap of
      Nothing -> lookupRec env
      Just typ -> pure typ

-- | `unify` takes in two types that we have judged to be equal, and updates
-- the state so that they are. It will fail if it judges that the two types
-- cannot be equal.
unify :: Type -> Type -> Inferrer Type
unify type1 type2 = do
  prnt $ "Unifying " ++ show type1 ++ ", " ++ show type2
  case (type1, type2) of
    (TypeVar name, t) -> do
      modEnv (\(TypeMap m) -> TypeMap $ fmap (replace name t) m) (remove name)
      return t
    (t, TypeVar name) -> unify type2 type1
    (t1 :=> t2, t3 :=> t4) -> do
      unify t1 t3 -- we need a way to see that the changes made here
      unify t2 t4 -- permeate down to the changes here! Maybe it's enough
                  -- just to have `unify` return a list of subs to be made?
    (NamedType name ts, NamedType name' ts') | name == name' ->
      NamedType name <$> mapM (uncurry unify) (zip ts ts')
    (_, _) -> error $
      "Incompatible types: " ++ show type1 ++
      " can't be unified with " ++ show type2
    where replace name newT origT = case origT of
            TypeVar n | n == name -> newT
            a :=> b -> replace name newT a :=> replace name newT b
            NamedType a ts -> NamedType a (replace name newT <$> ts)
            _ -> origT
          remove = S.delete

newVar :: Inferrer Type
newVar = give "a" where
  give name = do
    used <- getUsed
    if name `S.member` used then give $ next name
      else modUsed (S.insert name) >> return (TypeVar name)
  next name = let (c:cs) = reverse name in
    reverse (if c < 'z' then succ c : cs else 'a' : c : cs)

infer :: Expr -> Inferrer Type
infer expr = case expr of
  Number _ -> pure num
  String _ -> pure str
  Bool   _ -> pure bool
  Var name -> getVar name
  Apply func arg -> do
    argT <- infer arg
    funcT <- infer func
    returnT <- newVar
    res <- unify funcT (argT :=> returnT)
    return res
  Lambda name body -> do
    argT@(TypeVar var) <- newVar
    push (TypeMap $ M.singleton name argT) (S.singleton var)
    bodyT <- infer body
    argT' <- getVar name
    pop
    return $ argT' :=> bodyT
  Let name expr expr' -> do
    exprT <- infer expr
    modTmap (\(TypeMap m) -> TypeMap $ M.insert name exprT m)
    case expr' of
      Nothing -> return $ NamedType "" []
      Just e -> infer e

prnt = putStrLn ~> lift

start expr = (runStateT . infer) expr initEnv
  where initEnv = [(defaults, S.empty)]

test = grab ~> symsToVars ~> start

-- | Convenience function to convert all Symbols into Vars, so that we don't need
-- to worry about them when type checking (since they're equivalent in that regard)
symsToVars expr = case expr of
  Symbol s -> Var s
  If c t f -> If (symsToVars c) (symsToVars t) (symsToVars f)
  Let name e Nothing -> Let name (symsToVars e) Nothing
  Let name e (Just e') -> Let name (symsToVars e) (Just (symsToVars e'))
  Apply a b -> Apply (symsToVars a) (symsToVars b)
  Dotted a b -> Dotted (symsToVars a) (symsToVars b)
  Comma a b -> Comma (symsToVars a) (symsToVars b)
  Case e ms -> Case (symsToVars e)
                (map (\(e, e') -> (symsToVars e, symsToVars e')) ms)
  Tuple es -> Tuple $ map symsToVars es
  Lambda name e -> Lambda name (symsToVars e)
  List (ListLiteral l) -> List (ListLiteral $ map symsToVars l)
  List (ListRange a b) -> List (ListRange (symsToVars a) (symsToVars b))
  Datatype n ns cs (Just e) -> Datatype n ns cs (Just $ symsToVars e)
  e -> e

getUsed :: Inferrer UsedNames
getUsed = get >>= head ~> snd ~> pure

modUsed :: (UsedNames -> UsedNames) -> Inferrer ()
modUsed f = getUsed >>= f ~> pure >>= setUsed

setUsed :: UsedNames -> Inferrer ()
setUsed used = do
  (tmaps, _):env <- get
  put $ (tmaps, used):env

getTmap :: Inferrer TypeMap
getTmap = get >>= head ~> fst ~> pure

modTmap :: (TypeMap -> TypeMap) -> Inferrer ()
modTmap f = getTmap >>= f ~> pure >>= setTmap

setTmap :: TypeMap -> Inferrer ()
setTmap tmap = do
  (_, used):env <- get
  put $ (tmap, used):env

modEnv :: (TypeMap -> TypeMap) -> (UsedNames -> UsedNames) -> Inferrer ()
modEnv t u = modTmap t >> modUsed u

push :: TypeMap -> UsedNames -> Inferrer ()
push tmap used = do
  env <- get
  put $ (tmap, used):env

pop :: Inferrer (TypeMap, UsedNames)
pop = do
  (tmap, used):env <- get
  put env
  return (tmap, used)


defaults = TypeMap $ M.fromList $
  [
    ("+", num :=> num :=> num)
  ]


type TempSubs = M.Map Name Type
type Env' = ([TypeMap], UsedNames, [TempSubs])
type Inferrer' = StateT Env' IO

addUsed name = do
  (tms, u, tss):env <- get
  put (tms, S.insert name u, tss):env

removeUsed name = do
  (tms, u, tss):env <- get
  put (tms, S.delete name u, tss):env

pushTM = do
  (tms, u, tss):env <- get
  put (initTM:tms, u, tss):env

popTM = do
  (tm:tms, u, tss):env <- get
  put (tms, u, tss):env
  return tm

addTS name typ = do
  (tms, u, ts:tss):env <- get
  put (tms, u, M.insert name typ ts : tss):env

popTS = do
  (tms, u, ts:tss):env <- get
  put (tms, u, tss):env
  return ts

pushTS = do
  (tms, u, tss):env <- get
  put (tms, u, initTS:tss):env

unify' :: Type -> Type -> Inferrer' Type
unify' type1 type2 = do
  prnt $ "Unifying " ++ show type1 ++ ", " ++ show type2
  case (type1, type2) of
    (TypeVar name, t) -> do
      -- if name is in type subs, recurse with its substitution
      -- else add a substitution of name => t and return t
      -- need to figure out when to add to type map... maybe in
      -- a let statement, and that's it?
      -- cool thing is that without a specialization something will
      -- "stay" a typevar, which is exactly what we want if it's
      -- not restricted
    (t, TypeVar name) -> unify' type2 type1
    (t1 :=> t2, t3 :=> t4) -> unify' t1 t3 >> unify' t2 t4


    --(NamedType name ts, NamedType name' ts') | name == name' ->
    --  NamedType name <$> mapM (uncurry unify) (zip ts ts')
    --(_, _) -> error $
    --  "Incompatible types: " ++ show type1 ++
    --  " can't be unified with " ++ show type2
    --where replace name newT origT = case origT of
    --        TypeVar n | n == name -> newT
    --        a :=> b -> replace name newT a :=> replace name newT b
    --        NamedType a ts -> NamedType a (replace name newT <$> ts)
    --        _ -> origT
    --      remove = S.delete
