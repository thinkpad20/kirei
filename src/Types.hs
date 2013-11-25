module Types where

import Common

-------------------------------------------------------
-- Type checking
-------------------------------------------------------

newtype TypeEnv = TypeEnv (M.Map String Polytype)

instance Show TypeEnv where
  show (TypeEnv env) = let
    pairs = M.toList env
    toS (key, val) = show key ++ " => " ++ show val
    in "{" ++ (intercalate ", " $ map toS pairs) ++ "}"

instance Show TypeName where
  show (TypeName n []) = n
  show (TypeName n params) = intercalate " " (n: (show <$> params))

data Type =
  TVar String
  | BoolType
  | NumberType
  | StringType
  | TupleType [Type]
  | NamedType String [Type]
  | Type :=> Type
  deriving (Eq, Ord)

data Polytype = Polytype [String] Type
