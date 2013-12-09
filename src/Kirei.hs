-- Command-line utility to compile Kirei to JavaScript.
module Main where

import System.Environment (getArgs)
import CompileJS
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Control.Monad
import Control.Applicative ((<$>))
import TypeChecker
import Common

usage = "Usage: kirei <input filename(s)> [output filename]"
preamble = reqStd ++ imprts ++ "\n" where
  reqStd = "var std = require(\"./std\");\n"
  imprts = concatMap require stdObjs
  stdObjs = ["$IO", "Cons", "Empty", "add", "sub", "mult",
             "div", "lt", "gt", "leq", "geq", "eq", "neq",
             "and", "or", "neg", "mkList", "mkListRange",
             "defaultShow", "printLn", "print"]
  require o = concat ["var ", o, " = std.", o, ";\n"]

typeCheckFile :: FilePath -> IO ()
typeCheckFile path = do
  src <- readFile path
  env <- typeCheckEnv src
  putStrLn $ "Env: " ++ render 0 (tmap env)

compileAndWrite :: FilePath -> IO ()
compileAndWrite fname = do
  src <- readFile fname
  typeCheck src
  rendered <- renderJS src
  writeFile (getName fname) (preamble ++ rendered ++ "\n")
  putStrLn $ "Wrote output to " ++ getName fname
  where
    getName = splitOn "." ~> init ~> intercalate "." ~> (++ ".js")

main = do
  args <- getArgs
  case args of
    [] -> putStrLn usage
    (fname:_) -> compileAndWrite fname


