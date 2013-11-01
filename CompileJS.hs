import qualified JavaScript.AST as J
import Parser
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

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
toString s = (concat . map fromChar) s where
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

cTerm :: Term -> J.Term
cTerm (Bool b) = J.Bool b
cTerm (Num n) = J.Number n
cTerm (String s) = J.String s
cTerm (Identifier n) = J.Var n
cTerm (Symbol s) = J.Var $ toString s
cTerm (Parens e) = error $ "Shouldn't see parens"

type JS = StateT J.Statement Identity

compile :: Expr -> JS a
compile (Term t) = return $ cTerm t

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