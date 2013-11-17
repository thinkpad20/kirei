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

(~>) = flip (.)

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
  show s = let c = concat in case s of
    Expr e -> show e ++ ";"
    If' e b -> c ["if(", show e, "){", show b, "}"]
    If e b1 b2 -> c [show (If' e b1), "else", showElse b2] where
      showElse (Block [i@(If _ _ _)]) = " " ++ show i
      showElse _ = "{" ++ show b2 ++ "}"
    While e b -> "while(" ++ show e ++ "){" ++ show b ++ "}"
    For e1 e2 e3 b -> c ["for(", show e1,";", show e2, ";", show e3,
                             "){", show b, "}"]
    Assign (Var n) e -> "var " ++ n ++ "->" ++ show e ++ ";"
    Assign e e' -> show e ++ "->" ++ show e'
    Return' -> "return;"
    Return e -> "return " ++ show e ++ ";"
    Break -> "break;"
    Throw e -> "throw " ++ show e ++ ";"

instance Show Block where
  show (Block stmts) = concat $ map show stmts

instance Monoid Block where
  mempty = Block []
  mappend (Block a) (Block b) = Block $ a ++ b

indentation = 2

class Render a where
  render :: Int -> a -> String

instance Render Statement where
  render n stmt = concat $ case stmt of
    If' e b -> ["if (", render n e, ") {", rec b, sp n "}"]
    If e b1 b2 -> ["if (", render n e, ") {", rec b1, sp n "}",
      case b2 of
        Block [i@(If e block1 block2)] -> " else " ++ render n i
        _ -> " else {" ++ rec b2 ++ sp n "}"]
    While e b -> ["while (", render n e, ") {", rec b, sp n "}"]
    For e1 e2 e3 b -> ["for (", render n e1, ";", render n e2,
                         ";", render n e3, ") {", rec b, sp n "}"]
    Assign (Var v) e -> ["var ", v, " = ", render n e, ";"]
    Assign e e' -> [render n e, " = ", render n e', ";"]
    Return' -> ["return;"]
    Return e -> ["return ", render n e, ";"]
    Break -> ["break;"]
    Expr e -> [render n e, ";"]
    where sp n s = "\n" ++ replicate (n * indentation) ' ' ++ s
          rec (Block stmts) =
            sp (n+1) $ concatMap (render $ n+1) stmts


instance Render Block where
  render indent (Block stmts) = concat $ map (render indent) stmts

instance Render Expr where
  render n e = case e of
    Number n -> if isInt n then show $ floor n else show n
    Var n -> n
    String s -> show s
    This -> "this"
    Bool True -> "true"
    Bool False -> "false"
    Function ns blk -> c ["function (", sep ns, ") {",
                         sp (n+1) $ render (n+1) blk, sp n "}"]
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
