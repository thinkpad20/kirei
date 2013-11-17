-- Compiles Kirei to JavaScript. Will eventually be a REPL as well.

module Main where

import System.Environment (getArgs)
import CompileJS
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Control.Monad
import Control.Applicative ((<$>))

usage = "Usage: kirei <input filename(s)> [output filename]"
preamble = "var std = require(\"./std\");\nvar $IO = 0;\n"

compileAndWrite :: String -> IO ()
compileAndWrite fname = do
  src <- readFile fname
  writeFile (getName fname) (preamble ++ renderJS src)
  putStrLn $ "Wrote output to " ++ getName fname
  where
    getName = splitOn "." ~> init ~> intercalate "." ~> (++ ".js")

main = do
  args <- getArgs
  case args of
    [] -> putStrLn usage
    (fname:_) -> compileAndWrite fname


