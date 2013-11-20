module AlgorithmW where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import qualified Text.PrettyPrint as PP

data Exp =
  EVar String
  | ELit Lit
  | EApp Exp Exp
  | EAbs String Exp
  | ELet String Exp Exp
  deriving (Show, Eq, Ord)
data Lit =
  LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)
data Type =
  TVar String
  | TInt
  | TBool
  | TFun Type Type
  deriving (Show, Eq, Ord)

type Subst = M.Map String Type

class Types a where
  freeTypes :: a -> S.Set String
  apply :: Subst -> a -> a

instance Types Type where
  freeTypes t = case t of
    TVar name -> S.singleton name
    TInt -> S.empty
    TBool -> S.empty
    TFun t1 t2 -> freeTypes t1 `S.union` freeTypes t2
  apply subst t = case t of
    TVar name  -> case M.lookup name subst of
      Nothing     -> TVar name
      Just t      -> t
    TFun t1 t2 -> apply subst t1 `TFun` apply subst t2
    t          -> t

data Scheme = Scheme [String] Type deriving (Show)

instance Types Scheme where
  freeTypes (Scheme vars t) = freeTypes t `difference` S.fromList vars
  apply s (Scheme vars t) = Scheme vars $ apply (foldr M.delete s vars) t

instance Types a => Types [a] where
  apply s = map (apply s)
  freeTypes l = foldr S.union S.empty $ map freeTypes l
