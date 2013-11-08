import qualified JavaScript.AST as J
--import Parser3
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Data.Monoid

(<~) = (.)
(~>) = flip (.)

type Name = String

data Expr =
  Bool Bool
  | Number Double
  | String String
  | Symbol Name
  | Var Name
  | If Expr Expr Expr
  | Let Name Expr (Maybe Expr)
  | Apply Expr Expr
  | Lambda [Name] Expr
  deriving (Show)

toString :: String -> String
toString ">" = "gt"
toString "<" = "lt"
toString "=" = "eq"
toString ">=" = "geq"
toString "<=" = "leq"
toString "~" = "neg"
toString "+" = "add"
toString "-" = "sub"
toString "*" = "mult"
toString "/" = "div"
toString "&&" = "and"
toString "||" = "or"
toString "!" = "not"
toString s = (map fromChar ~> concat) s where
  fromChar '>' = "gt"
  fromChar '<' = "lt"
  fromChar '=' = "eq"
  fromChar '~' = "tilde"
  fromChar '+' = "plus"
  fromChar '-' = "minus"
  fromChar '*' = "star"
  fromChar '/' = "fslash"
  fromChar '\\' = "bslash"
  fromChar '&' = "amp"
  fromChar '|' = "pipe"
  fromChar '!' = "bang"

returnIt = J.single . J.Return . J.Term

-- | Should make a separate function for top-level statements,
--   so that it doesn't return

eToBlk :: Expr -> J.Block
eToBlk (If c t f) = J.single $ J.If (eToE c) (eToBlk t) (eToBlk f)
eToBlk (Let v e e') = case e' of
  Nothing -> J.single $ J.Assign v $ eToE e
  Just e' -> eToBlk (Let v e Nothing) <> eToBlk e'
eToBlk (Lambda xs e) = returnIt $ J.Function xs $ eToBlk e
eToBlk (Apply a b) = J.single $ J.Return $ J.Call (eToE a) [eToE b]
eToBlk e = J.single $ J.Return $ eToE e

eToE :: Expr -> J.Expr
eToE (Bool b) = J.Term $ J.Bool b
eToE (Number n) = J.Term $ J.Number n
eToE (String s) = J.Term $ J.String s
eToE (Var v) = J.Term $ J.Var v
eToE (Symbol s) = J.Term $ J.Var $ toString s
eToE (If c t f) = J.Ternary (eToE c) (eToE t) (eToE f)
eToE (Let v e e') = error $ "Let statement in an expression"
eToE (Lambda xs e) = J.Term $ J.Function xs $ eToBlk e
eToE (Apply a b) = J.Call (eToE a) [eToE b]

--cTerm :: Term -> J.Term
--cTerm (Bool b) = J.Bool b
--cTerm (Number n) = J.Number n
--cTerm (String s) = J.String s
--cTerm (Variable name) = J.Var name
--cTerm (Symbol s) = J.Var $ toString s
--cTerm (Parens e) = error $ "Shouldn't see parens"
--cTerm (Lambda vs ls e) = J.Function vs $ letsToBlk ls <> (eToBlk e)

--letsToBlk :: [Let] -> J.Block
--letsToBlk = undefined

--toList :: a -> [a]
--toList a = [a]

--eToBlk :: Expr -> J.Block
--eToBlk (Term t) = J.single $ J.Return $ Just $ J.Term $ cTerm t
--eToBlk (Apply e1 e2) = J.single $ J.Expr $ J.Call (eToE e1) [eToE e2]
--eToBlk (If c t f) = J.single $ J.If (eToE c) (eToBlk t) (Just $ eToBlk f)
--eToBlk (Comma e1 e2) = J.single (J.Expr $ eToE e1) <> eToBlk e2

--eToE :: Expr -> J.Expr
--eToE = undefined

--cStmt :: Statement -> J.Statement
--cStmt (Expr e) = J.Term $ cTerm t
--cStmt (Assign (Let v e)) = J.Assign n e

--type JS = StateT J.Statement Identity

--compile :: Expr -> JS a
--compile (Term t) = return $ cTerm t

{-
OK, the problem is that JavaScript sees things as a list of statements,
while our language sees things as a single expression. So for example,
for an if statement in JS, we have an expression and two blocks. But
in an if statement here, we simply have three expressions. It's not
possible to just make three recursive calls to the same function: clearly,
we need to be able to return either a single statement, a single expression,
a list of statements or a list of expressions. Figuring out when we need
each one, we can probably construct the appropriate transformation. But
this clearly requires some thought.
-}


--exprToExpr :: Expr -> J.Expr
--exprToExprs :: Expr -> [J.Expr]
--exprToStatement :: Expr -> J.Statement
--exprToStatements :: Expr -> [J.Statement]

{-

Alternatively, we could use a JSNode-type structure which encapsulates terms,
expressions, statements, and blocks...

Or we could use a state monad type of deal... the state variable is a list
of statements, and the contained variables are things that we just
generated... hmm...
-}



--cExpr :: Expr -> Bool -> J.Statement
--cExpr (Term term) ok = if ok
--                         then J.Return $ Just $ J.Term $ cTerm term
--                         else J.Expr $ J.Term $ cTerm term
--cExpr (Apply e1 e2) ok = if ok
--                           then J.Return $ Just $ J.Call (cExpr e1) [cExpr e2]
--                           else J.Expr $ J.Call (cExpr e1) [cExpr e2]
--cExpr (If c t f) _ = J.If (cExpr c False) blk1 (Just blk2) where
--                        blk1 = J.Block [J.Expr $ cExpr t True]
--                        blk2 = J.Block [J.Expr $ cExpr f True]
--cExpr (Lambda ns e) True = J.Return $ Just $ J.Term $ J.Function ns $ cExpr e
--cExpr (Lambda ns e) False = J.Expr $ J.Term $ J.Function ns $ cExpr e
--cExpr (Let n e (Just e')) = S $ J.Assign n $ cExpr e

{-
So here's what we figured out. We want to return if:
  - It's not an if statement
  - It's not a let statement
  - It's not in the condition of an if
  - It's not in the right branch of a function application
-}
