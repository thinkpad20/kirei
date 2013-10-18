module AST where

import qualified Data.Map as M

type Name = String

type Env = M.Map String Expr

data Statement = ImportStmt Import
               | LetStmt [Assign]
               | DataDec DataDec
               | Expression Expr
               deriving (Show, Eq, Ord)

data Import = Import [[Name]]
            | From [Name] [Name]
            deriving (Show, Eq, Ord)

data Assign = Let [Dec] deriving (Show, Eq, Ord)

data Dec = Dec Name [Param] Expr deriving (Show, Eq, Ord)

data DataDec = Data Name [Constructor] deriving (Show, Eq, Ord)

data Constructor = Constructor Name [Name] deriving (Show, Eq, Ord)

data Param = Param String
           | Pattern [Term]
           deriving (Show, Eq, Ord)

data Expr = Term Term
          | Call Expr Expr
          | If Expr Expr Expr
          | Stmt Statement Expr
          deriving (Show, Eq, Ord)

data Term = Num Double
          | VarName Name
          | String String
          | Lambda [Name] Expr Env
          | List List
          | Dict (M.Map Expr Expr)
          deriving (Show, Eq, Ord)

data List = JustList [Expr]
          | ListComp [Expr] [Name] Expr
          deriving (Show, Eq, Ord)

plus, minus, times, divide :: Expr -> Expr -> Expr
plus e1 e2 = Call (Call (Term $ VarName "+") e1) e2
minus e1 e2 = Call (Call (Term $ VarName "-") e1) e2
times e1 e2 = Call (Call (Term $ VarName "*") e1) e2
divide e1 e2 = Call (Call (Term $ VarName "/") e1) e2