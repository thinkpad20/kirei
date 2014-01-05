module CompileJS (toJs, renderJS, ppJS, grab) where

import qualified JavaScript.AST as J
import Parser
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Control.Applicative
import Debug.Trace
import Data.Char (toLower)
import Prelude hiding (foldr)
import Common
import AST
import Types hiding (nameStack, records)
import qualified Data.Map as M
import qualified Data.Set as S

type Compiler      = ErrorT CompilerError (StateT CompilerState IO)
type CompilerError = String

data CompilerState =
  CompilerState { nameStack   :: [Name]
                , records     :: M.Map Name Type
                , overloadeds :: S.Set Name
                } deriving Show

runCompiler :: Compiler a
            -> M.Map Name Type
            -> IO (Either CompilerError a, CompilerState)
runCompiler compiler records = flip runStateT initialState $ runErrorT compiler
  where initialState = CompilerState { nameStack   = ["(root)"]
                                     , records     = records
                                     , overloadeds = mempty
                                     }

-- Exporting functions
toJs :: String -> IO J.Block
toJs src = do
  expr     <- grab src
  (compiled, _) <- runCompiler (compile expr) mempty
  case compiled of
    Left err    -> error err
    Right block -> return block

renderJS :: String -> IO String
renderJS src = toJs src >>= return . (render 0)

ppJS :: String -> IO ()
ppJS src = renderJS src >>= putStrLn

single e = J.Block [e]

boolAndAssigns :: J.Expr -> Expr -> (Maybe J.Expr, J.Block)
boolAndAssigns argName expr = (bools, assignments) where
  (bools, assignments, _) = ba 0 argName expr
  ba :: Int -> J.Expr -> Expr -> (Maybe J.Expr, J.Block, Int)
  ba k argName expr = case expr of
    Number n ->(eq' J.Number n, mempty, k)
    String s -> (eq' J.String s, mempty, k)
    TypeName "True" -> (eq' J.Bool True, mempty, k)
    TypeName "False" -> (eq' J.Bool False, mempty, k)
    TypeName n -> (eq (aRef 0) (J.String n), mempty, 1)
    Var v -> case argName of
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
    otherwise -> error $ "Unhandled case: " ++ show expr
    where eq e1 e2 = Just $ J.Binary "===" e1 e2
          eq' f a = eq argName $ f a
          aRef n = J.ArrayReference argName (J.Number n)
          (Just e1) `and` (Just e2) = Just $ J.Binary "&&" e1 e2
          a `and` b = a `mplus` b
          -- make an AND of all the a==b expressions in subcompilations
          fst (a, b, c) = a
          snd (a, b, c) = b
          mkBool list = foldr and Nothing (fst <$> list)
          -- concatenate all of the assignments from the subcompilations
          mkAssigns list = mconcat (snd <$> list)

compileCase :: Int -> Expr -> Matches -> Compiler J.Block
compileCase tempNum expr matches = case expr of
  (Var name) -> matchesToBlock (J.Var name) matches
  otherwise  -> do
    assign <- J.Assign mkVName <$> eToE expr
    mappend (single assign) <$> matchesToBlock mkVName matches
  where mkVName = J.Var $ "__temp" ++ show tempNum

compileCaseToExpr :: Expr -> Matches -> Compiler J.Expr
compileCaseToExpr expr matches = do
  blocks <- matchesToBlock (J.Var "_a") matches
  let call = J.Call (J.Function ["_a"] blocks)
  call <$> (pure <$> eToE expr)

matchesToBlock :: J.Expr -> Matches -> Compiler J.Block
matchesToBlock v matches = case matches of
  [] -> error "Empty case statement with no matches"
  (pat, res):ms -> case boolAndAssigns v pat of
    (Nothing, J.Block []) -> eToBlk res
    (Nothing, assignments) -> mappend assignments <$> eToBlk res
    (Just cond, assignments) -> do
      resBlock <- eToBlk res
      nextBlock <- case ms of
        [] -> return $ single $ J.throwNewError "Pattern match failed"
        _  -> matchesToBlock v ms
      return $ single (J.If cond (assignments <> resBlock) nextBlock)


makeConstructors :: Name -> [Constructor] -> J.Block
makeConstructors name cs = mconcat (construct ~> single <$> cs) where
  tName (TVar _ v) = v
  tName (TConst "[]") = "list"
  tName (TConst c) = map toLower c
  tName (TTuple ts) = concatMap tName ts
  tName (a :=> b) = tName a ++ "to" ++ tName b
  tName (TApply a b) = tName a ++ tName b
  mkNames ts = zipWith (++) (tName <$> ts) (show <$> [0..])
  construct :: Constructor -> J.Statement
  construct (Constructor name types) = case types of
    [] -> J.Assign (mkVName name) (J.Array [J.String name])
    ts -> J.Assign (mkVName name) $ func (mkNames ts) where
      vars = J.Var <$> mkNames ts
      func [] = J.Array (J.String name : vars)
      func (name:names) = J.Function [name] $ single $ J.Return $ func names
  mkVName "[]" = J.Var "Empty"
  mkVName name = J.Var $ if isSymbol name then toString name else name


-- Compilation
-- eToBlk compiles an expression to a block; this means that it will always
-- end with a return statement.
eToBlk :: Expr -> Compiler J.Block
eToBlk expr = case expr of
  If c t f -> single <$> (J.If <$> eToE c <*> eToBlk t <*> eToBlk f)
  Let name e e' -> case e' of
    -- if there's no next expression, make a singleton block
    Nothing -> single <$> J.Assign (J.Var $ makeName name) <$> eToE e
    -- otherwise, do the same but then append on the next guy
    Just e' -> mappend <$> eToBlk (Let name e Nothing) <*> eToBlk e'
  Apply a (Tuple es) -> call <$> eToE a <*> mapM eToE es
  Apply a b -> call <$> eToE a <*> (pure <$> eToE b)
  Comma e1 e2 -> mappend <$> compile e1 <*> eToBlk e2
  Case expr matches -> compileCase 0 expr matches
  Datatype name tnames cs e' -> case e' of
    Nothing -> return $ makeConstructors name cs
    Just e' -> mappend (makeConstructors name cs) <$> eToBlk e'
  e -> single <$> J.Return <$> eToE e
  where call e es = single $ J.Return $ J.Call e es
        makeName name | isSymbol name = toString name
                      | otherwise = name

-- eToE compiles an expression to a JS expression. This is used when we
-- are inside of another expression, or some other situation where we don't
-- need to be producing a full block. As such, encountering a let statement
-- or a comma here is either syntactically invalid, or should never occur
-- due to the algorithm.
eToE :: Expr -> Compiler J.Expr
eToE expr = case expr of
  Number n -> return $ J.Number n
  String s -> return $ J.String s
  Tuple es -> J.Array <$> mapM eToE es
  TypeName "[]" -> return $ J.Var "Empty"
  TypeName "True" -> return $ J.Bool True
  TypeName "False" -> return $ J.Bool False
  TypeName n -> return $ J.Var $ if isSymbol n then toString n else n
  Var v -> return $ J.Var v
  Symbol s -> return $ J.Var $ toString s
  If c t f -> J.Ternary <$> eToE c <*> eToE t <*> eToE f
  Lambda (Var x) e -> J.Function [x] <$> eToBlk e
  Lambda (Tuple exprs) e -> J.Function (map toArg exprs) <$> eToBlk e
    where toArg (Var x) = x
          toArg _ = error $ "Can't handle arbitrary exprs in args yet"
  Apply a (Tuple es) -> J.Call <$> eToE a <*> (mapM eToE es)
  Apply a b -> J.Call <$> eToE a <*> (pure <$> eToE b)
  Dotted e1 e2 -> J.Dot <$> eToE e1 <*> eToE e2
  Case expr matches -> compileCaseToExpr expr matches
  List (ListLiteral list) -> J.Call (J.Var "mkList") <$> mapM eToE list
  List (ListRange start stop) ->
    J.Call (J.Var "__mkListRange__") <$> mapM eToE [start, stop]
  l@(Let name expr next) -> case next of
    Nothing ->
      throwError $ "Unfinished let statement in an expression: " ++ show l
    Just next ->
      J.Call <$> (J.Function [name] <$> eToBlk next) <*> (pure <$> eToE expr)
  e@(Comma _ _) ->
    throwError $ "Comma in an expression: " ++ show e
  otherwise ->
    error $ "FATAL: Unhandlable expression: " ++ show expr

-- compile is the top-level compilation function. It's almost identical to
-- eToBlk except that it does not produce a return on bare expressions or
-- if statements.
compile :: Expr -> Compiler J.Block
compile expr = case expr of
  If c t f -> single <$> (J.If <$> eToE c <*> compile t <*> compile f)
  Let v e e' -> case e' of
    Nothing -> single <$> J.Assign (J.Var v) <$> eToE e
    Just e' -> mappend <$> eToBlk (Let v e Nothing) <*> compile e'
  Apply a (Tuple exprs) -> call <$> eToE a <*> mapM eToE exprs
  Apply a b -> call <$> eToE a <*> (pure <$> eToE b)
  Comma l@(Let v e e') e2 -> mappend <$> compile l <*> compile e2
  Comma e1 e2 -> mappend <$> compile e1 <*> compile e2
  Datatype name tnames cs e' -> case e' of
    Nothing -> return $ makeConstructors name cs
    Just e' -> mappend (makeConstructors name cs) <$> compile e'
  -- any other expression types will be treated as single expressions
  e -> single <$> J.Expr <$> eToE e
  where call e es = single $ J.Expr $ J.Call e es

runTest :: IO ()
runTest = do
  src <- readFile "../lib/first.kr"
  ppJS src

-- Utility functions

toString :: String -> String
toString ">" = "gt"
toString "<" = "lt"
toString "==" = "eq"
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
toString "::" = "cons"
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
  fromChar ':' = "col"

test = ppJS
