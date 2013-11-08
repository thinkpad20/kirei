module AST where

data Sig = Sig String TypeSig deriving (Show)

data TypeSig = TypeName TypeName
             | MapFrom TypeSig TypeSig
             deriving (Show)

type TypeName = String

data FuncDef = FuncDef {
  signature :: (Maybe TypeSig), 
  name :: String,
  args :: [String],
  body :: Expr
} deriving (Show)

data Expr = Term Term
          | Unary String Expr
          | Binary String Expr Expr
          | Apply Expr Expr
          | Assign String Expr
          deriving (Show)

data Term = Number Double
          | Name String
          deriving (Show)