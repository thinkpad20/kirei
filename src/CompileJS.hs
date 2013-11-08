module CompileJS where

import qualified JavaScript.AST as J
import Parser
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Data.Monoid

(<~) = (.)
(~>) = flip (.)

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

-- | Should make a separate function for top-level statements,
--   so that it doesn't return

eToBlk :: Expr -> J.Block
eToBlk (If c t f) = J.single $ J.If (eToE c) (eToBlk t) (eToBlk f)
eToBlk (Let v e e') = case e' of
  Nothing -> J.single $ J.Assign v $ eToE e
  Just e' -> eToBlk (Let v e Nothing) <> eToBlk e'
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

toJs = grab ~> eToBlk
