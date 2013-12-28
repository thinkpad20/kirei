{-# LANGUAGE LambdaCase #-}
module KindChecker (infer, KindSubs, (•), unify, apply) where

import Prelude hiding (foldr)
import Common
import Parser
import Types hiding (Types(..), (•))
import qualified Data.Map as M
import qualified Data.Set as S

type KindMap = M.Map Name Kind
type KindSubs = KindMap

-- | runs inferrence but throws an error if there are any kind variables left
--inferFullKind :: KindMap -> Type -> Inferrer (KindSubs, Kind)
--inferFullKind env typ = do
--  (subs, kind) <- infer env typ
--  (subs', kind') <- check (apply subs kind)

--  where
--    only  kind = return (mempty, kind)
--    check kind = case kind of
--      Type -> only kind
--      kind1 :-> kind2 -> do
--        (subs1, kind1') <- check kind1
--        (subs2, kind2') <- check (apply subs1 kind2)
--        return (subs1 • subs2, kind1' :-> kind2')
--      KVar _ -> do
--        subs <- unify kind Type
--        return (subs, apply subs kind)

-- | an adaptation of algorithm W to checking kinds.
infer :: KindMap -> Type -> Inferrer (KindSubs, Kind)
infer env typ = case typ of
  TConst name -> M.lookup name <$> gets kinds >>= \case
    Nothing -> throwError $ "Unknown type constant `" ++ name ++ "`"
    Just kind -> only kind
  TVar classes name -> case M.lookup name env of
    Nothing -> throwError $ "Type variable `" ++ name ++ "` is out of scope"
    Just kind ->
      if length classes > 0
      then throwError $ "No class restrictions allowed in constructors"
      else only kind
  TApply funcT argT -> do
    resultK        <- newvar
    (funcS, funcK) <- infer env funcT
    (argS, argK)   <- infer (apply funcS <$> env) argT
    unifyS         <- unify funcK (argK :-> resultK)
                        `catchError` applyError funcT argT
    return (unifyS • argS • funcS, apply unifyS resultK)
  type1 :=> type2 -> do
    (subs1, kind1) <- infer env type1
    (subs2, kind2) <- infer (apply subs1 <$> env) type2
    unifyS <- unify (apply (subs1 • subs2) kind1) Type `catchError` functionError
    unifyS' <- unify (apply (subs1 • subs2) kind2) Type `catchError` functionError
    return (unifyS • unifyS' • subs1 • subs2, Type)
  TTuple types -> do
    subsAndKinds <- mapM (infer env) types
    let subs = foldr (•) mempty (map fst subsAndKinds)
    let kinds = map (snd ~> apply subs) subsAndKinds
    unifySubs <- mapM (unify Type) kinds `catchError` tupleError
    return (foldr (•) subs unifySubs, Type)
  where
    only kind = return (mempty, kind)
    newvar = newTypeVar [] >>= \(TVar _ name) -> return $ KVar name

unify :: Kind -> Kind -> Inferrer KindSubs
unify kind1 kind2 = case (kind1, kind2) of
  (KVar name, kind) -> return $ M.singleton name kind
  (kind, KVar name) -> return $ M.singleton name kind
  (Type, Type) -> return mempty
  (k1 :-> k2, k3 :-> k4) -> do
    subs1 <- unify k1 k3
    subs2 <- apply subs1 k2 `unify` apply subs1 k4
    return (subs1 • subs2)
  (k1, k2) -> throwError $ "Kinds do not unify: " ++ render 0 k1 ++ " !: " ++
                   render 0 k2

subs1 • subs2 = (apply subs1 <$> subs2) `M.union` subs1

apply subs kind = case kind of
  Type -> Type
  KVar name -> case M.lookup name subs of
    Nothing -> KVar name
    Just kind -> kind
  kind1 :-> kind2 -> apply subs kind1 :-> apply subs kind2

prnt :: String -> Inferrer ()
prnt = lift . lift . putStrLn

applyError funcT argT msg = throwError $
  "Error when attempting to apply `" ++ render 0 funcT ++ "` to `" ++
  render 0 argT ++ "`. Message was: " ++ msg
functionError msg = throwError $
  "Functions must map from complete types to complete types. " ++
  "Encountered error: " ++ msg
tupleError msg = throwError $
  "Type tuples must only contain complete types. Encountered error: " ++ msg
