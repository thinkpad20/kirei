-- Somewhat hacky JavaScript AST
module JavaScript.AST where

import Data.List
import Data.Monoid
import Control.Applicative

type Name = String
newtype Block = Block [Statement]

data Statement = Expr Expr
               | If Expr Block Block
               | If' Expr Block
               | While Expr Block
               | For Expr Expr Expr Block
               | Assign String Expr
               | Return Expr
               | Return'
               | Break

data Expr = Term Term
          | Dot Expr Expr
          | Call Expr [Expr]
          | Binary String Expr Expr
          | Unary String Expr
          | Ternary Expr Expr Expr

data Term = Bool Bool
          | Number Double
          | Var Name
          | String String
          | Array [Expr]
          | This
          | Function [Name] Block
          | Parens Expr


instance Show Term where
  show (Number n) = show n
  show (Var n) = n
  show (String s) = show s
  show This = "this"
  show (Function ns blk) = c ["function(", sep ns, "){", show blk, "}"]
    where { c = concat; sep = intercalate "," }
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Array exprs) = c ["[", sep $ map show exprs, "]"]
    where { c = concat; sep = intercalate "," }

instance Show Expr where
  show (Term term) = show term
  show (Dot e1 e2) = show e1 ++ "." ++ show e2
  show (Call e es) = concat [show e, "(", intercalate "," (map show es), ")"]
  show (Binary op e1 e2) = concat [show e1, op, show e2]
  show (Unary op e) = op ++ show e
  show (Ternary e1 e2 e3) = concat ["(", show e1, "?", show e2,
                                    ":", show e3, ")"]

instance Show Statement where
  show (Expr e) = show e ++ ";"
  show (If' e b) = concat ["if(", show e, "){", show b, "}"]
  show (If e b1 b2) = concat [show (If' e b1), "else{", show b2, "}"]
  show (While e b) = "while(" ++ show e ++ "){" ++ show b ++ "}"
  show (For e1 e2 e3 b) = concat ["for(", show e1,";", show e2, ";", show e3,
                           "){", show b, "}"]
  show (Assign n e) = "var " ++ n ++ "=" ++ show e ++ ";"
  show Return' = "return;"
  show (Return e) = "return " ++ show e ++ ";"
  show Break = "break;"

instance Show Block where
  show (Block stmts) = concat $ map show stmts

instance Monoid Block where
  mempty = Block []
  mappend (Block a) (Block b) = Block $ a ++ b

single :: Statement -> Block
single s = Block [s]

indentation = 2

class Render a where
  render :: Int -> a -> String

instance Render Block where
  render indent (Block stmts) = c $ map (r indent) stmts where
    c = concat
    sp n s = replicate (n * indentation) ' ' ++ s
    rec :: Int -> Block -> String
    rec n (Block stmts) = "\n" ++ c (map (r (n + 1)) stmts)
    r :: Int -> Statement -> String
    r n (If' e b) = c [sp n "if (", render n e, ") {", rec n b, sp n "}"]
    r n (If e b1 b2) = c [r n (If' e b1), sp n "else {", rec n b2, sp n "}"]
    r n (While e b) = c [sp n "while (", render n e, ") {", rec n b, sp n "}"]
    r n (For e1 e2 e3 b) = c [sp n "for (", render n e1, ";", render n e2, ";",
                              render n e3, ") {", rec n b, sp n "}"]
    r n (Assign v e) = c [sp n "var ", v, " = ", render n e, ";\n"]
    r n Return' = sp n "return;"
    r n (Return e) = sp n "return " ++ render n e ++ ";"
    r n Break = sp n "break;"
    r n (Expr e) = c [sp n $ render n e, ";\n"]

instance Render Term where
  render _ (Number n) = show n
  render _ (Var n) = n
  render _ (String s) = show s
  render _ This = "this"
  render _ (Bool True) = "true"
  render _ (Bool False) = "false"
  render n (Function ns blk) = c ["function (",
                                 intercalate ", " ns,
                                 ") {\n", render (n+1) blk, sp n "}"]
    where c = concat
          sp n s = "\n" ++ replicate (n * indentation) ' ' ++ s
  render n (Array exprs) = c ["[", sep $ map (render n) exprs, "]"]
    where { c = concat; sep = intercalate "," }

instance Render Expr where
  render n e = case e of
    Term term -> render n term
    Dot e1 e2 -> render n e1 ++ "." ++ render n e2
    Call e es -> c [render n e, "(", intercalate ", " (map (render n) es), ")"]
    Binary op e1 e2 -> c [render n e1, " ", op, " ", render n e2]
    Unary op e -> op ++ render n e
    Ternary e1 e2 e3 -> c ["(", render n e1, " ? ", render n e2, " : ",
                           render n e3, ")"]
    where c = concat
