module CompileJS where

import qualified JavaScript.AST as J
import Parser
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Data.Monoid

(~>) = flip (.)
infixr 9 ~>

preamble = "var std = require(\"./std\");\n\n"

toString :: String -> String
toString ">" = "std.gt"
toString "<" = "std.lt"
toString "=" = "std.eq"
toString ">=" = "std.geq"
toString "<=" = "std.leq"
toString "~" = "std.neg"
toString "+" = "std.add"
toString "-" = "std.sub"
toString "*" = "std.mult"
toString "/" = "std.div"
toString "&&" = "std.and"
toString "||" = "std.or"
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
  fromChar '$' = "dol"

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

-- Used for top-level statements only
compile :: Expr -> J.Block
compile (If c t f) = J.single $ J.If (eToE c) (compile t) (compile f)
compile (Let v e e') = case e' of
  Nothing -> J.single $ J.Assign v $ eToE e
  Just e' -> eToBlk (Let v e Nothing) <> compile e'
compile (Apply a b) = J.single $ J.Expr $ J.Call (eToE a) [eToE b]
compile e = J.single $ J.Expr $ eToE e

toJs = grab ~> compile

renderJS = toJs ~> J.render 0
