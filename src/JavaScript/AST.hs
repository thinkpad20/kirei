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
               | Assign Expr Expr
               | Return Expr
               | Return'
               | Throw Expr
               | Break

data Expr = Bool Bool
          | Number Double
          | Var Name
          | String String
          | Array [Expr]
          | This
          | Function [Name] Block
          | Parens Expr
          | Dot Expr Expr
          | Call Expr [Expr]
          | ArrayReference Expr Expr
          | Binary String Expr Expr
          | Unary String Expr
          | Ternary Expr Expr Expr
          | New Expr

instance Show Expr where
  show t = case t of
    Number n -> if isInt n then show $ floor n else show n
    Var n -> n
    String s -> show s
    This -> "this"
    Function ns blk -> c ["function(", sep ns, "){", show blk, "}"]
    Bool True -> "true"
    Bool False -> "false"
    Array exprs -> c ["[", sep $ map show exprs, "]"]
    Dot e1 e2 -> show e1 ++ "." ++ show e2
    Call e es -> c [show e, "(", intercalate "," (map show es), ")"]
    Binary op e1 e2 -> c [show e1, op, show e2]
    Unary op e -> op ++ show e
    Ternary e1 e2 e3 -> c ["(", show e1, "?", show e2,
                                    ":", show e3, ")"]
    ArrayReference e1 e2 -> show e1 ++ "[" ++ show e2 ++ "]"
    New e -> "new " ++ show e
    where c   = concat
          sep = intercalate ","

instance Show Statement where
  show (Expr e) = show e ++ ";"
  show (If' e b) = concat ["if(", show e, "){", show b, "}"]
  show (If e b1 b2) = concat [show (If' e b1), "else{", show b2, "}"]
  show (While e b) = "while(" ++ show e ++ "){" ++ show b ++ "}"
  show (For e1 e2 e3 b) = concat ["for(", show e1,";", show e2, ";", show e3,
                           "){", show b, "}"]
  show (Assign (Var n) e) = "var " ++ n ++ "=" ++ show e ++ ";"
  show (Assign e e') = show e ++ "=" ++ show e'
  show Return' = "return;"
  show (Return e) = "return " ++ show e ++ ";"
  show Break = "break;"
  show (Throw e) = "throw " ++ show e ++ ";"

instance Show Block where
  show (Block stmts) = concat $ map show stmts

instance Monoid Block where
  mempty = Block []
  mappend (Block a) (Block b) = Block $ a ++ b

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
    r n (If' e b) = c [sp n "if (", render n e, ") {", rec n b, "\n", sp n "}"]
    r n (If e b1 b2) = c [r n (If' e b1), "\n", sp n "else {", rec n b2, "\n", sp n "}"]
    r n (While e b) = c [sp n "while (", render n e, ") {",
                         rec n b, "\n", sp n "}"]
    r n (For e1 e2 e3 b) = c [sp n "for (", render n e1, ";", render n e2,
                              ";", render n e3, ") {", rec n b, "\n", sp n "}"]
    r n (Assign (Var v) e) = c [sp n "var ", v, " = ", render n e, ";\n"]
    r n (Assign e e') = c [render n e, " = ", render n e', ";\n"]
    r n Return' = sp n "return;"
    r n (Return e) = sp n "return " ++ render n e ++ ";"
    r n Break = sp n "break;"
    r n (Expr e) = c [sp n $ render n e, ";\n"]

instance Render Expr where
  render n e = case e of
    Number n -> if isInt n then show $ floor n else show n
    Var n -> n
    String s -> show s
    This -> "this"
    Bool True -> "true"
    Bool False -> "false"
    Function ns blk -> c ["function (", sep ns, ") {\n",
                         render (n+1) blk, sp n "}"]
    Array exprs -> c ["[", sep $ map (render n) exprs, "]"]
    Dot e1 e2 -> render n e1 ++ "." ++ render n e2
    Call e es -> c [render n e, "(", intercalate ", " (map (render n) es), ")"]
    ArrayReference e e' -> c [render n e, "[", render n e', "]"]
    Binary op e1 e2 -> c [render n e1, " ", op, " ", render n e2]
    Unary op e -> op ++ render n e
    Ternary e1 e2 e3 -> c ["(", render n e1, " ? ", render n e2, " : ",
                           render n e3, ")"]
    where c = concat
          sep = intercalate ","
          sp n s = "\n" ++ replicate (n * indentation) ' ' ++ s

--Returns if x is an int to n decimal places
isIntTo :: (Integral a, RealFrac b) => b -> a -> Bool
isIntTo x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0

isInt x = isIntTo x 10

throwNewError msg = Throw $ New $ Call (Var "Error") [String msg]
