module TypeChecker where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intersect, (\\), foldl')
import AST
import Parser
import Common

data Type =
  TypeVar Name            -- e.g. `a` in `foo : a -> (a, String)` or `List a`
  | NamedType Name [Type] -- e.g. `String` or `List Int`
  | Type :=> Type         -- functions
  deriving (Eq, Ord)

infixr 4 :=>

data Scheme = Scheme [Name] Type deriving (Show)

data TypeEnvironment = TE (M.Map Name Scheme)
type UsedNames = S.Set Name
type Substitutions = M.Map Name Type
type Env = UsedNames

type TypeChecker = StateT Env IO

class Types a where
  free :: a -> S.Set Name
  apply :: Substitutions -> a -> a

instance Types Type where
  free (TypeVar name) = S.singleton name
  free (NamedType name ts) = unionAll (free <$> ts)
  free (t1 :=> t2) = free t1 `S.union` free t2

  apply subs t@(TypeVar name) = M.findWithDefault t name subs
  apply subs (NamedType name ts) = NamedType name $ fmap (apply subs) ts
  apply subs (a :=> b) = apply subs a :=> apply subs b

instance Types Scheme where
  free (Scheme vars t) = (free t) S.\\ S.fromList vars
  apply subs (Scheme vars t) = Scheme vars (apply subs t)

instance Types TypeEnvironment where
  free (TE env) = unionAll (free <$> M.elems env)
  apply subs (TE env) = TE $ apply subs <$> env

num  = NamedType "Number" []
str  = NamedType "String" []
bool = NamedType "Bool" []
tuple = NamedType ""

unionAll = foldl' S.union S.empty
none = M.empty
getUsed = get
addUsed name = modify (S.insert name)

(•) = M.union

generalize :: TypeEnvironment -> Type -> Scheme
generalize env t = Scheme (S.toList $ free t S.\\ free env) t

instantiate :: Scheme -> TypeChecker Type
instantiate (Scheme vars term) = do
  newVars <- mapM makename vars
  return $ apply (M.fromList  $ zip vars newVars) term

newvar :: TypeChecker Type
newvar = makename "a"

makename :: Name -> TypeChecker Type
makename name = getUsed >>= \s -> case S.member name s of
  False -> addUsed name >> pure (TypeVar name)
  True -> let (c:cs) = reverse name in
    makename $ reverse (if c < 'z' then succ c:cs else 'a':c:cs)

algorithmW :: TypeEnvironment -> Expr -> TypeChecker (Substitutions, Type)
algorithmW te@(TE env) expr = prnt ("inferring " ++ show expr) >> case expr of
  Number _    -> return (none, num)
  String _    -> return (none, str)
  Bool _      -> return (none, bool)
  Var x       -> case M.lookup x env of
    Nothing -> error $ "Undefined variable `" ++ x ++ "`"
    Just t  -> instantiate t >>= \t' -> return (none, t')
  Tuple exprs -> do
    subsAndTypes <- mapM (algorithmW te) exprs
    return (foldl' (•) none (fst <$> subsAndTypes), tuple $ snd <$> subsAndTypes)
  Lambda x e  -> do
    β        <- newvar
    (s1, t1) <- algorithmW (TE $ M.insert x (Scheme [] β) env) e
    return (s1, apply s1 β :=> t1)
  Apply e1 e2 -> do
    (s1, t1) <- algorithmW te e1
    (s2, t2) <- algorithmW (apply s1 te) e2
    β        <- newvar
    s3       <- (apply s2 t1) `unify` (t2 :=> β)
    return (s2 • s2 • s1, apply s3 β)
  Let x e1 e2 -> do
    (s1, t1) <- algorithmW te e1
    case e2 of
      Nothing -> return (s1, NamedType "" [])
      Just e2 -> do
        let env' = TE $ M.insert x (generalize (apply s1 te) t1) env
        (s2, t2) <- algorithmW env' e2
        return (s2 • s1, t2)

unify :: Type -> Type -> TypeChecker Substitutions
a `unify` b = do
  case (a,b) of
    (l :=> r, l' :=> r') -> do
      s1 <- l `unify` l'
      s2 <- apply s1 r `unify` apply s1 r'
      return (s1 • s2)
    (TypeVar u, t) -> u `bind` t
    (t, TypeVar u) -> u `bind` t
    (NamedType n ts, NamedType n' ts')
      | n == n' -> do
        subList <- mapM (uncurry unify) (zip ts ts')
        return (foldl' (•) none subList)
      | otherwise -> error $ "Named type mismatch: " ++ n ++ " !: " ++ n'
    (t1, t2) -> error $ "types do not unify: " ++ show t1 ++ " !: " ++ show t2
  where
    bind name typ | typ == TypeVar name = return none
                  | name `S.member` free typ = error $
                      "Error: " ++ name ++ " occurs free in type " ++ show typ
                  | otherwise = return (M.singleton name typ)


initials = TE $ M.fromList
  [
    ("+", lit $ num :=> num :=> num),
    ("-", lit $ num :=> num :=> num),
    ("*", lit $ num :=> num :=> num),
    ("/", lit $ num :=> num :=> num)
  ]
  where lit = Scheme []

prnt = lift . putStrLn
runInferrer = flip runStateT
infer = algorithmW initials ~> runInferrer S.empty
test input = do
  ((subs, typ), env) <- input ! grab ! symsToVars ! infer
  return typ

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

instance Show Type where
  show t = case t of
    TypeVar name -> name
    NamedType "" ts -> "(" ++ intercalate ", " (map show ts) ++ ")"
    NamedType name [] -> name
    NamedType name ts -> name ++ " " ++ (intercalate " " $ map show' ts)
    t1 :=> t2 -> show' t1 ++ " -> " ++ show t2
    where show' t@(NamedType (_:_) (_:_)) = "(" ++ show t ++ ")"
          show' t@(a :=> b) = "(" ++ show t ++ ")"
          show' t = show t
