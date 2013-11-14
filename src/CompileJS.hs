module CompileJS where

import qualified JavaScript.AST as J
import Parser
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Data.Monoid
import Control.Applicative

(~>) = flip (.)
infixr 9 ~>

-- Exporting functions
toJs :: String -> J.Block
toJs = grab ~> compile

renderJS :: String -> String
renderJS = toJs ~> J.render 0

prettyPrintJS :: String -> IO ()
prettyPrintJS = renderJS ~> putStrLn

-- Unraveling pattern matching...
-- This should be able to express all of the patterns we support
data Pattern =
  BoolConst Bool
  | StrConst String
  | NumConst Double
  | VarPattern String
  | TuplePattern [Pattern]
  | Constructor String [Pattern]
  deriving (Show)

-- Now we need to convert an Expr into a Pattern
eToPattern :: Expr -> Pattern
eToPattern expr = case expr of
  Bool b -> BoolConst b
  Number n -> NumConst n
  String s -> StrConst s
  Var v -> VarPattern v
  Symbol s -> VarPattern s
  Tuple exprs -> TuplePattern (eToPattern <$> exprs)
  Apply (Var v) e -> Constructor v [eToPattern e]
  Apply (Symbol s) e -> Constructor s [eToPattern e]
  Apply a b -> case eToPattern a of
    Constructor s exs -> Constructor s (exs ++ [eToPattern b])
    _ -> error $ "wtf: " ++ show (eToPattern a)
  e -> error $ "Invalid pattern: " ++ show e

testPat = grab ~> eToPattern

-- Compilation
-- eToBlk compiles an expression to a block; this means that it will always
-- end with a return statement.
eToBlk :: Expr -> J.Block
eToBlk expr = case expr of
  If c t f -> J.single $ J.If (eToE c) (eToBlk t) (eToBlk f)
  Let v e e' -> case e' of
    Nothing -> J.single $ J.Assign v $ eToE e
    Just e' -> eToBlk (Let v e Nothing) <> eToBlk e'
  Apply a (Tuple es) -> call (eToE a) (eToE <$> es)
  Apply a b -> call (eToE a) [eToE b]
  Comma e1 e2 -> compile e1 <> eToBlk e2
  e -> J.single $ J.Return $ eToE e
  where call e es = J.single $ J.Return $ J.Call e es

-- eToE compiles an expression to a JS expression. This is used when we
-- are inside of another expression, or some other situation where we don't
-- need to be producing a full block. As such, encountering a let statement
-- or a comma here is either syntactically invalid, or should never occur
-- due to the algorithm.
eToE :: Expr -> J.Expr
eToE expr = case expr of
  Bool b -> J.Term $ J.Bool b
  Number n -> J.Term $ J.Number n
  String s -> J.Term $ J.String s
  Tuple es -> J.Term $ J.Array (eToE <$> es)
  Var v -> J.Term $ J.Var v
  Symbol s -> J.Term $ J.Var $ toString s
  If c t f -> J.Ternary (eToE c) (eToE t) (eToE f)
  Lambda x e -> J.Term $ J.Function [x] $ eToBlk e
  Apply a (Tuple es) -> J.Call (eToE a) (eToE <$> es)
  Apply a b -> J.Call (eToE a) [eToE b]
  Dotted e1 e2 -> J.Dot (eToE e1) (eToE e2)
  l@(Let v e e') -> error $ "Let statement in an expression: " ++ show l
  e@(Comma _ _) -> error $ "Comma in an expression: " ++ show e

-- compile is the top-level compilation function. It's almost identical to
-- eToBlk except that it does not produce a return on bare expressions or
-- if statements.
compile :: Expr -> J.Block
compile expr = case expr of
  If c t f -> J.single $ J.If (eToE c) (compile t) (compile f)
  Let v e e' -> case e' of
    Nothing -> J.single $ J.Assign v $ eToE e
    Just e' -> eToBlk (Let v e Nothing) <> compile e'
  Apply a (Tuple es) -> call (eToE a) (eToE <$> es)
  Apply a b -> call (eToE a) [eToE b]
  Comma l@(Let v e e') e2 -> compile l <> compile e2
  Comma e1 e2 -> compile e1 <> compile e2
  e -> J.single $ J.Expr $ eToE e
  where call e es = J.single $ J.Expr $ J.Call e es

runTest :: IO ()
runTest = do
  src <- readFile "../lib/first.kr"
  prettyPrintJS src

-- Utility functions

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
  fromChar '@' = "at"
