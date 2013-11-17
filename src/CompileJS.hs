module CompileJS where

import qualified JavaScript.AST as J
import Parser
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Data.Monoid
import Control.Applicative
import Debug.Trace

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

-- Unraveling pattern matching...
-- This should be able to express all of the patterns we support
--data Pattern =
--  BoolConst Bool
--  | StrConst String
--  | NumConst Double
--  | VarPattern String
--  | TuplePattern [Pattern]
--  | Constructor String [Pattern]
--  deriving (Show)

---- Now we need to convert an Expr into a Pattern
--eToPattern :: Expr -> Pattern
--eToPattern expr = case expr of
--  Bool b -> BoolConst b
--  Number n -> NumConst n
--  String s -> StrConst s
--  Var v -> VarPattern v
--  Symbol s -> VarPattern s
--  Tuple exprs -> TuplePattern (eToPattern <$> exprs)
--  Apply (Var v) e -> Constructor v [eToPattern e]
--  Apply (Symbol s) e -> Constructor s [eToPattern e]
--  Apply a b -> case eToPattern a of
--    Constructor s exs -> Constructor s (exs ++ [eToPattern b])
--    _ -> error $ "wtf: " ++ show (eToPattern a)
--  e -> error $ "Invalid pattern: " ++ show e

--testPat = grab ~> eToPattern

{-

# when matching against a variable, we need arrays
let foo y =
  let x = bar y;
  case x of
    (b, "hello", 2) -> "oh noes"
  | (a, b, c) -> "sweet";

var foo = function (y) {
  var x = bar(y);
  if (x[1] === "hello" && x[2] === 2) {
    return "oh noes";
  }
  else {
    return "sweet";
  }
};

# when matching with arguments we need named arguments
let baz x = case x of
  (1, 2, 3) -> 4
| (a, b, c) -> a + b - c;

# this is the same case as
let baz (1, 2, 3) = 4
|   baz (a, b, c) = a + b - c;

# and the same case as
let baz (a, b, c) = case (a, b, c) of
  (1, 2, 3) -> 4
| else -> a + b - c;

# and all should compile to
var baz = function(_arg0, _arg1, _arg2) {
  if (_arg0 === 1 && _arg1 === 2 && _arg2 === 3) {
    return 4;
  }
  else {
    var a = _arg0;
    var b = _arg1;
    var c = _arg2;
    return a + b - c;
  }
};

Essentially we have two cases: one, when the tuple in the case statement is
actually the arguments to the function, in which case, we use those arguments
in our boolean expression. In the second case, the tuple is the result of a
function call, or some local variable.

Within the first case, there are two sub-possibilities: that we were provided
argument names, and that the tuple was declared with a variable.

Let's assume for now that we can distinguish the two easily at compile time.
Most of the functionality is going to be the same, so let's say that we
can pass in a "TupleMode", described below, to distinguish the case we're in

There is a third, weirder case, when the tuple is simply created "on the fly",
in which case it might refer to anything:

let foo (a, b) c =
  let bar = someFunctionCall;
  case (b, bar, c) of
    (1, 2, 3) -> 4
  | (x, y, z) -> 7 * x - y * z;

Or try this on:

let foo (a, b, c) =
  let bar = (5*b, c*7);
  case bar of
    (5, 6) -> 3
  | (b, c) -> b + c;

In this instance, it's a local variable, but it shows that things can get
kinda weird and soon really not clear. We'll probably need to be aware of
types and context to really be robust in this.
-}

testBAS name = grab ~> boolAndAssigns name

single e = J.Block [e]

--mkMatch :: TupleMode -> [(Expr, Expr)] -> J.Block
--mkMatch _ [] = J.Block []
--mkMatch mode ((e, e'):es) = let
{-
let foo x = case x of () -> 1 | (a, b) -> a + b;
var foo = function () {
  if (arguments.length == 0) {
    return 1;
  }
  else if (arguments[0] ==)
}

Ok sweet looks like javascript makes this easy for us. We can always use the
arguments array in a match statement, regardless of what the declared variable
names are! Yay. Of course, local vs argument is still an important question,
because it changes the variable name we're pulling from... but that just means
we can pass in `arguments` as the name of the variable (and it becomes a local
variable)!

OK so let's say that we have

(a, b, 1)

and the argname is arguments.

then we should call
[boolAndAssigns arguments[0] a,
 boolAndAssigns arguments[1] b,
 boolAndAssigns arguments[2] 1]
so if we had
[(0, a), (1, b), (2, 1)]

we'd get back
[arguments]

-}

boolAndAssigns :: J.Expr -> Expr -> (Maybe J.Expr, J.Block)
boolAndAssigns argName expr = case expr of
  Bool b -> (eq' J.Bool b, mempty)
  Number n ->(eq' J.Number n, mempty)
  String s -> (eq' J.String s, mempty)
  Var v -> case argName of
    J.Var v' | v' == v -> (Nothing, mempty)
    _ -> (Nothing, single $ J.Assign (J.Var v) argName)
  Tuple exprs -> (bool, assigns) where
    -- take an index i and make an expression argName[i]
    newArgName i = J.ArrayReference argName (J.Number i)
    -- will compile a match with argName[i] as the argName
    convertWithIndex (i, e) = boolAndAssigns (newArgName i) e
    -- does pair conversion and converts each
    list = convertWithIndex <$> zip [0..] exprs
    andStep (Just e1) (Just e2) = Just $ J.Binary "&&" e1 e2
    andStep a b = a `mplus` b
    -- make an AND of all the a==b expressions in subcompilations
    bool = foldr andStep Nothing (fst <$> list)
    -- concatenate all of the assignments from the subcompilations
    assigns = mconcat (snd <$> list)
  where eq e1 e2 = Just $ J.Binary "===" e1 e2
        eq' f a = eq argName (f a)

-- OK let's go the next level up, something that takes a pair
-- of expressions and turns it into an if statement.
compileMatch :: J.Expr -> (Expr, Expr) -> J.Statement
compileMatch var (pat, res) = case boolAndAssigns var pat of
  -- if there's no filtering to be done, we can just return the variable
  (Nothing, J.Block []) -> J.Expr var
  (Just bool, assigns) -> J.If' bool (assigns <> eToBlk res)

{-
let foo x y = case (x * y, someOtherVar) of
  (3, 4) -> 5
| (a, b) -> a ^ b;
-}

{-
case foo of
  bar -> baz;

var bar = foo;
return baz;

case foo of
  1 -> bar;

if (foo == 1) {
  return bar;
}


-}

-- alright sweet so the next step up is to take a list of expression pairs
-- and produce a series of 0 or more if statements. A tricky thing here
-- is that we need to be able to handle ifs and elses correctly.
compileCase :: Int -> Expr -> Matches -> J.Block
compileCase tempNum expr matches = case expr of
  (Var name) -> doIt (J.Var name) matches
  _ -> single (J.Assign mkVName (eToE expr)) <> doIt (eToE expr) matches
  where mkVName = J.Var $ "__temp" ++ show tempNum
        doIt :: J.Expr -> Matches -> J.Block
        doIt v matches = case matches of
          [] -> error "Empty case statement with no matches"
          (pat, res):ms -> case boolAndAssigns v pat of
            (Nothing, J.Block []) -> eToBlk res
            (Nothing, assignments) -> assignments <> eToBlk res
            (Just cond, assignments) -> single $
              J.If cond (assignments <> eToBlk res) $ case ms of
                [] -> single $ J.throwNewError "Pattern match failed"
                _ -> doIt v ms

compileCase' :: String -> J.Block
compileCase' input = case grab input of
  Case expr matches -> compileCase 0 expr matches
  otherwise -> error $ "That's not a case, it's a " ++ show (grab input)

-- Alright so now we want to compile a case statement... We need
-- as input the name of the variable to match against. If the case
-- is matching against a function call, then we need to store the
-- result in another variable


-- OK what we actually want to do is take an (Expr, Expr) pair and
-- produce a JS if-statement.
--makeMatch :: String -> (Expr, Expr) -> J.Statement
--makeMatch argName (pattern, result) = J.If bool block where
--  eq e1 e2 = Just $ J.Binary "===" e1 e2
--  eq' f a = eq (J.Var argName) (f a)
--  _and = J.Binary "&&"
--  boolAndAssigns :: Name -> Expr -> (J.Bool, [J.Statement])
--  boolAndAssigns arg pat = case pat of
--    Bool b -> (eq' J.Bool b, [])
--    Number n ->(eq' J.Number n, [])
--    String s -> (eq' J.String s, [])
--    Var v -> (Nothing, [J.Assign v (J.Var arg)])
--    Tuple es -> let ba = boolAndAssigns <$> es
--    -- gotta go, but at a high level with tuples and constructors
--    -- we want to recurse down, if the pattern we recursed to is
--    -- nothing then we don't do anything with it, otherwise && all
--    -- of the patterns that we got together into a big boolean
--    _ -> error $ "Invalid pattern match " ++ show pattern
--  (bool, assignments) = boolAndAssigns argName
--  block = toBlock blkInfo
--  toBlock = undefined

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
