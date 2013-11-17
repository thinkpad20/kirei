module CompileJS where

import qualified JavaScript.AST as J
import Parser
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Data.Monoid
import Control.Applicative
import Debug.Trace
import Data.Char
import Data.List

(~>) = flip (.)
infixr 9 ~>

(!) = flip ($)

-- Exporting functions
toJs :: String -> J.Block
toJs = grab ~> compile

renderJS :: String -> String
renderJS = toJs ~> J.render 0

prettyPrintJS :: String -> IO ()
prettyPrintJS = renderJS ~> putStrLn
ppJS = prettyPrintJS

testBAS name = grab ~> boolAndAssigns name

single e = J.Block [e]

boolAndAssigns :: J.Expr -> Expr -> (Maybe J.Expr, J.Block)
boolAndAssigns argName expr = (bools, assignments) where
  (bools, assignments, _) = ba 0 argName expr
  ba :: Int -> J.Expr -> Expr -> (Maybe J.Expr, J.Block, Int)
  ba k argName expr = case expr of
    Bool b -> (eq' J.Bool b, mempty, k)
    Number n ->(eq' J.Number n, mempty, k)
    String s -> (eq' J.String s, mempty, k)
    Var v -> if isConstructor v then (eq (aRef 0) (J.String v), mempty, 1)
      else case argName of
        J.Var v' | v' == v -> (Nothing, mempty, k)
        _ -> (Nothing, single $ J.Assign (J.Var v) argName, k)
    Symbol s -> (eq (aRef 0) (J.String s), mempty, 1)
    Tuple exprs -> (mkBool list, mkAssigns list, k) where
      -- take an index i and make an expression argName[i]
      newArgName i = J.ArrayReference argName (J.Number i)
      -- will compile a match with argName[i] as the argName
      convertWithIndex (i, e) = ba k (newArgName i) e
      -- does pair conversion and converts each
      list = convertWithIndex <$> zip [0..] exprs
    Apply a b -> let
      (b1, a1, i) = ba k argName a
      (b2, a2, j) = ba i (aRef $ fromIntegral i) b
      in (b1 `and` b2, a1 <> a2, j+1)
    where eq e1 e2 = Just $ J.Binary "===" e1 e2
          eq' f a = eq argName $ f a
          aRef n = J.ArrayReference argName (J.Number n)
          isConstructor (c:_) = isUpper c
          (Just e1) `and` (Just e2) = Just $ J.Binary "&&" e1 e2
          a `and` b = a `mplus` b
          -- make an AND of all the a==b expressions in subcompilations
          fst (a, b, c) = a
          snd (a, b, c) = b
          mkBool list = foldr and Nothing (fst <$> list)
          -- concatenate all of the assignments from the subcompilations
          mkAssigns list = mconcat (snd <$> list)

compileCase :: Int -> Expr -> Matches -> J.Block
compileCase tempNum expr matches = case expr of
  (Var name) -> matchesToBlock (J.Var name) matches
  _ -> single (J.Assign mkVName (eToE expr)) <> matchesToBlock mkVName matches
  where mkVName = J.Var $ "__temp" ++ show tempNum

compileCaseToExpr :: Expr -> Matches -> J.Expr
compileCaseToExpr expr matches =
  J.Call (J.Function ["_a"] $ matchesToBlock (J.Var "_a") matches) [eToE expr]

matchesToBlock :: J.Expr -> Matches -> J.Block
matchesToBlock v matches = case matches of
  [] -> error "Empty case statement with no matches"
  (pat, res):ms -> case boolAndAssigns v pat of
    (Nothing, J.Block []) -> eToBlk res
    (Nothing, assignments) -> assignments <> eToBlk res
    (Just cond, assignments) -> single $
      J.If cond (assignments <> eToBlk res) $ case ms of
        [] -> single $ J.throwNewError "Pattern match failed"
        _ -> matchesToBlock v ms


makeConstructors :: Name -> [Constructor] -> J.Block
makeConstructors name cs = mconcat (construct ~> single <$> cs) where
  tName :: TypeName -> String
  tName (TypeName name []) = toLower <$> name
  tName (TypeName name ts) = toLower <$> name ++ concatMap tName ts
  construct :: Constructor -> J.Statement
  construct (Constructor name types) = case types of
    [] -> J.Assign (J.Var name) (J.Array [J.String name])
    ts -> J.Assign (J.Var name) $ func (tName <$> ts) where
      toVars = J.Var . tName <$> ts
      func [] = J.Array (J.String name : toVars)
      func (name:names) = J.Function [name] $ single $ J.Return $ func names

-- Compilation
-- eToBlk compiles an expression to a block; this means that it will always
-- end with a return statement.
eToBlk :: Expr -> J.Block
eToBlk expr = case expr of
  If c t f -> single $ J.If (eToE c) (eToBlk t) (eToBlk f)
  Let v e e' -> case e' of
    Nothing -> single $ J.Assign (J.Var v) $ eToE e
    Just e' -> eToBlk (Let v e Nothing) <> eToBlk e'
  Apply a (Tuple es) -> call (eToE a) (eToE <$> es)
  Apply a b -> call (eToE a) [eToE b]
  Comma e1 e2 -> compile e1 <> eToBlk e2
  Case expr matches -> compileCase 0 expr matches
  Datatype name cs e' -> case e' of
    Nothing -> makeConstructors name cs
    Just e' -> makeConstructors name cs <> eToBlk e'
  e -> single $ J.Return $ eToE e
  where call e es = single $ J.Return $ J.Call e es

-- eToE compiles an expression to a JS expression. This is used when we
-- are inside of another expression, or some other situation where we don't
-- need to be producing a full block. As such, encountering a let statement
-- or a comma here is either syntactically invalid, or should never occur
-- due to the algorithm.
eToE :: Expr -> J.Expr
eToE expr = case expr of
  Bool b -> J.Bool b
  Number n -> J.Number n
  String s -> J.String s
  Tuple es -> J.Array (eToE <$> es)
  Var v -> J.Var v
  Symbol s -> J.Var $ toString s
  If c t f -> J.Ternary (eToE c) (eToE t) (eToE f)
  Lambda x e -> J.Function [x] $ eToBlk e
  Apply a (Tuple es) -> J.Call (eToE a) (eToE <$> es)
  Apply a b -> J.Call (eToE a) [eToE b]
  Dotted e1 e2 -> J.Dot (eToE e1) (eToE e2)
  Case expr matches -> compileCaseToExpr expr matches
  List (ListLiteral list) -> J.Call (J.Var "mkList") (eToE <$> list)
  List (ListRange start stop) ->
    J.Call (J.Var "mkListRange") $ eToE <$> [start, stop]
  l@(Let v e e') -> error $ "Let statement in an expression: " ++ show l
  e@(Comma _ _) -> error $ "Comma in an expression: " ++ show e

-- compile is the top-level compilation function. It's almost identical to
-- eToBlk except that it does not produce a return on bare expressions or
-- if statements.
compile :: Expr -> J.Block
compile expr = case expr of
  If c t f -> single $ J.If (eToE c) (compile t) (compile f)
  Let v e e' -> case e' of
    Nothing -> single $ J.Assign (J.Var v) $ eToE e
    Just e' -> eToBlk (Let v e Nothing) <> compile e'
  Apply a (Tuple es) -> call (eToE a) (eToE <$> es)
  Apply a b -> call (eToE a) [eToE b]
  Comma l@(Let v e e') e2 -> compile l <> compile e2
  Comma e1 e2 -> compile e1 <> compile e2
  Datatype name cs e' -> case e' of
    Nothing -> makeConstructors name cs
    Just e' -> makeConstructors name cs <> compile e'
  e -> single $ J.Expr $ eToE e
  where call e es = single $ J.Expr $ J.Call e es

runTest :: IO ()
runTest = do
  src <- readFile "../lib/first.kr"
  prettyPrintJS src

-- Utility functions

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
toString "^" = "pow"
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
