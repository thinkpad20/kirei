-- Somewhat hacky JavaScript AST
module JavaScript.AST where
  
import Data.List

type Name = String
newtype Block = Block [Statement]

data Statement = Expr Expr
               | If Expr Block (Maybe Block)
               | While Expr Block
               | For Expr Expr Expr Block
               | Assign String Expr
               | Return (Maybe Expr)
               | Break

data Expr = Term Term
          | Dot [Term]
          | Call Expr [Expr]
          | Binary String Expr Expr
          | Unary String Expr

data Term = Bool Bool
          | Number Double
          | Var Name
          | String String
          | This
          | Function [Name] Block
          | Parens Expr

instance Show Term where
  show (Number n) = show n
  show (Var n) = n
  show (String s) = show s
  show This = "this"
  show (Function ns blk) = concat ["function (", 
                                 intercalate ", " ns, 
                                 ") {", show blk, "}"]
  show (Bool True) = "true"
  show (Bool False) = "false"

instance Show Expr where
  show (Term term) = show term
  show (Dot terms) = intercalate "." (map show terms)
  show (Call e es) = concat [show e, "(", intercalate ", " (map show es), ")"]
  show (Binary op e1 e2) = concat [show e1, " ", op, " ", show e2]
  show (Unary op e) = op ++ show e

instance Show Statement where
  show (Expr e) = show e ++ ";"
  show (If e b Nothing) = concat ["if (", show e, ") {", show b, "}"]
  show (If e b1 (Just b2)) = concat [show (If e b1 Nothing), 
                                     " else {", show b2, "}"]
  show (While e b) = "while (" ++ show e ++ ") {" ++ show b ++ "}"
  show (For e1 e2 e3 b) = concat ["for (", show e1,";", show e2, ";", show e3, 
                           ") {", show b, "}"]
  show (Assign n e) = n ++ " = " ++ show e ++ ";"
  show (Return Nothing) = "return;"
  show (Return (Just e)) = "return " ++ show e ++ ";"
  show Break = "break;"

instance Show Block where
  show (Block stmts) = concat $ map show stmts