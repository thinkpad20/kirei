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

testBAS name = grab ~> boolAndAssigns name

single e = J.Block [e]

{-
let reverse l =
  case l of
    Empty -> []
  | (1 :: bs) -> [2]
  | (2 :: 3 :: _) -> [5,6,7]
  | (a :: as) -> (reverse as) ++ a;

Var "Empty"
Apply
  Apply
    Symbol "::"
    Number 1
  Var "bs"

Apply
  Apply
    Apply
      Apply
        Symbol "::" <- l[0]
        Number 2 <- l[1]
      Number 3 <- l[2]
    Symbol "::"
  Underscore

e0 = Apply (Symbol "::")
e1 = Apply (Symbol "::") (Number 2)
e2 = Apply

start with l
Apply
  Apply <- l
    Symbol "::" <- l[0]
    Number 2.0 <- l[1]
  Apply <- l[2]
    Apply
      Symbol "::" <- l[2][0]
      Number 3.0 <- l[2][1]
    Var "Empty" <- l[2][2]

# start with l
Apply
  Apply
    Symbol "::" <- l[0] # argh
    Number 2.0 <- l[1]
  Apply # right side gets l[2]
    Apply # see an apply
      Symbol "::" <- l[2][0]
      Number 3.0 <- l[2][1]
    Apply
      Apply
        Symbol "::" <- l[2][2][0]
        Number 4.0 <- l[2][2][1]
      Var "Empty" <- l[2][2][2]

OK even worse, what if we have a datatype that takes more than 2 args?
datatype Foo = Bar | Foo Int Int Foo;
let f = Foo 1 2 (Foo 3 4 Bar)
JS representation:
["Foo", 1, 2, ["Foo", 3, 4, ["Bar"]]]
AST for this:
Apply <- depth of "applys" tells us how many elements are in the constructor!
  Apply
    Apply <- 3 "Apply"s, so we know Foo has 3 elements
      Var "Foo" <- f[0] <- this is f[0] always, we're good
      Number 1.0 <- f[1]
    Number 2.0 <- f[2] <- we know this is f[2] because... of the previous call
  Apply <- when we see an Apply as a SECOND argument, we're entering another constructor
           If we're entering another constructor, that's when we have to
           make a new base variable, in this case f[3], since the previous
           pattern returned with 2 as its highest argument
    Apply <- which will have k arguments... can we use this info? maybe it's
             interesting but not necessary to code into the algorithm...
      Apply
        Var "Foo" <- f[3][0] <- we hit that constructor and we do the same pattern...
        Number 3.0 <- f[3][1]
      Number 4.0 <- f[3][2]
    Var "Bar" <- f[3][3][0]

And EVEN worse, what about something that can recurse at multiple levels?
datatype Qux = End | Qux Qux Qux Int;
let qux = Qux (Qux End End 3) (Qux (Qux End End 5) End 6) 7;
["Qux",
  ["Qux",
    ["End"],
    ["End"],
    3],
  ["Qux",
    ["Qux",
      ["End"],
      ["End"],
      5],
    ["End"],
    6],
  7]
]

What we can see from here:
- the first argument in every Apply is either:
  - a constructor, or
  - another Apply.

So what does that tell us? In the first case, we
want to look at the 0th element of whatever the
relevant array is:

  Apply (Var v) e -> let
    constructorCheck = eq (aRef 0) (J.String v)
    (bool, assigns) = boolsAndAssigns (aRef 1) e
    in (constructorCheck `andStep` bool, assigns)

compile with _a, 0
Apply
  Var "Foo"
  Number 1
-> (_a[0] === "Foo" && _a[1] === 1, 1)

  (Apply
    (Apply
      (Apply
        (Var "Foo") <- f[3][0] -> ([_a[3][0] === "Foo"], [], 0)
        (Var "b")) <- f[3][1] -> ([], [Assign "b" _a[3][1]], 1)
      (Var "c")) <- f[3][2] -> ([_a[3][0] === "Foo"], [Assign "b" _a[3][1], Assign "c" f[3][2]], 2)
    (Var "Bar")) <- f[3][3][0] -> ([_a[3][0] === "Foo", _a[3][3][0] === "Bar"], [Assign "b" _a[3][1], Assign "c" f[3][2]], 2)

Apply
  (Apply
    (Apply
      (Var "Foo") <- f[0]
      (Number 1.0) <- f[1]
    (Number 3.0) <- f[2]
  (Apply
    (Apply
      (Apply
        (Var "Foo") <- f[3][0]
        (Var "b")) <- f[3][1]
      (Var "c")) <- f[3][2]
    (Var "Bar")) <- f[3][3][0]

Apply
  (Apply
    (Apply
      (Var "Qux") <- l[0]
      (Apply <- second argument is an apply, so we need to "go deeper"
                meaning l -> l[1] (we just finished the 0th index)
        (Apply <- first arg is an Apply -> recurse down l[1]
          (Apply <- first arg is a Var -> compile that with l[1], get ([l[1][0]==="Qux"], [], 0)
                    second arg is a Var, constructor -> compile with l[1], get ([l[1][1][0]] === "End", [], 1)
                    finally return ()
            (Var "Qux") <- l[1][0] -> ([l[1][0]==="Qux"], [], 0)
            (Var "End")) <- l[1][1][0] -> ([l[1][1][0]] === "End", [], 1)
          (Var "End")) <- l[1][2][0]
        (Number 3.0))) <- l[1][3]
    (Apply
      (Apply
        (Apply
          (Var "Qux") <- l[2][0]
          (Apply
            (Apply
              (Apply
                (Var "Qux") <- l[2][]
                (Var "End"))
              (Var "End"))
            (Number 5.0)))
        (Var "End"))
      (Number 6.0)))
    (Number 7.0)
-}

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
  e -> single $ J.Expr $ eToE e
  where call e es = single $ J.Expr $ J.Call e es

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
