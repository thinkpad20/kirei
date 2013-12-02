module TypeChecker where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import AST
import Parser
import Types
import Common

type UsedNames = S.Set Name
type Env = (TypeEnvironment, UsedNames)

type TypeChecker = StateT Env IO

num  = NamedType "Number" []
str  = NamedType "String" []
bool = NamedType "Bool" []
tuple = NamedType ""

getUsed :: TypeChecker UsedNames
getUsed = get

addUsed :: Name -> TypeChecker ()
addUsed name = modify (S.insert name)

freeUsed :: Name -> TypeChecker ()
freeUsed name = modify (S.delete name)

none = M.empty
(•)  = M.union

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

infer :: TypeEnvironment -> Expr -> TypeChecker (Substitutions, Type)
infer te@(TE env) expr = case expr of
  Number _ -> return (none, num)
  String _ -> return (none, str)
  Bool   _ -> return (none, bool)
  Var    x -> case M.lookup x env of
    -- this feels kinda hacky, but it seems to work, otherwise recursion doesn't
    Nothing -> do
      prnt $ x ++ " is nothin'"
      newvar >>= \v -> return (M.singleton x v, v)  -- error $ "Undefined variable `" ++ x ++ "`"
    Just t  -> do
      prnt $ x ++ " is just " ++ show t
      instantiate t >>= \t' -> return (none, t')
  Tuple exprs -> do
    subsAndTypes <- mapM (infer te) exprs
    return (foldl' (•) none (fst <$> subsAndTypes), tuple $ snd <$> subsAndTypes)
  Lambda (Var x) e  -> do
    β        <- newvar
    (s1, t1) <- infer (TE $ M.insert x (Scheme [] β) env) e
    return (s1, apply s1 β :=> t1)
  Lambda p e -> do
    (pSubs, pType) <- infer te p
    (eSubs, eType) <- infer (apply pSubs te) e
    return (eSubs • pSubs, eType)
  Sig name typ e -> let sub = M.singleton name typ in case e of
    Nothing -> return (sub, tuple [])
    Just e -> infer (apply sub te) e
  Apply e1 e2 -> do
    prnt $ "inferring (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
    (s1, t1) <- infer te e1
    prnt $ "e1: " ++ show e1 ++ " : " ++ show (s1, t1)
    (s2, t2) <- infer (apply s1 te) e2
    prnt $ "e2: " ++ show e2 ++ " : " ++ show (s2,t2)
    β        <- newvar
    s3       <- (apply s2 t1) `unify` (t2 :=> β)
    prnt $ "final type " ++ show (apply s3 β)
    prnt $ "final subs " ++ show (s2 • s2 • s1)
    return (s2 • s2 • s1, apply s3 β)
  Let var Nothing e1 e2 -> error $ "Type of " ++ var ++ " can't be inferred"
  Let var (Just typ) e1 e2 -> do
    (subs1, type1) <- infer te e1
    subs2 <- unify typ type1
    case e2 of
      Nothing -> return (subs1, NamedType "" [])
      Just e2 -> do
        let env' = TE $ M.insert var (generalize (apply subs1 te) type1) env
        (subs2, type2) <- infer env' e2
        return (subs2 • subs1, type2)
  If c t f -> do
    (cSubs, cType) <- infer te c
    cSubs'         <- cType `unify` bool
    (tSubs, tType) <- infer (apply (cSubs' • cSubs) te) t
    (fSubs, fType) <- infer (apply (tSubs • cSubs' • cSubs) te) f
    finalSubs      <- tType `unify` fType
    return (finalSubs • fSubs • tSubs • cSubs' • cSubs, tType)
  c@(Case e matches) -> infer te $ caseToLambda c

unify :: Type -> Type -> TypeChecker Substitutions
unify a b = do
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
    --("+", lit $ num :=> num :=> num),
    --("-", lit $ num :=> num :=> num),
    --("*", lit $ num :=> num :=> num),
    --("/", lit $ num :=> num :=> num),
    --("<", lit $ num :=> num :=> bool),
    --(">", lit $ num :=> num :=> bool),
    --("<=", lit $ num :=> num :=> bool),
    --(">=", lit $ num :=> num :=> bool),
    --("&&", lit $ bool :=> bool :=> bool),
    --("not", lit $ bool :=> bool),
    --("__matchFail__", witha a),
    ("__matchError__", witha a),
    ("[-]", witha $ a :=> a :=> a),
    ("Empty", witha $ n "List" [a]),
    ("::", witha $ a :=> n "List" [a] :=> n "List" [a]),
    ("undefined", witha a)
  ]
  where lit = Scheme []
        witha = Scheme ["a"]
        a = TypeVar "a"
        cons = Scheme ["a"]
        n = NamedType

prnt :: String -> TypeChecker ()
prnt = lift . putStrLn
runInfer = infer initials ~> flip runStateT (S.singleton "a")
test input = do
  ((subs, typ), used) <- input ! grab ! symsToVars ! runInfer
  putStrLn $ input ++ "\nis of type\n" ++ show typ
  print ((subs, typ), used)
