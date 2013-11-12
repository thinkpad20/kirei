-- Compiles Kirei to JavaScript. Will eventually be a REPL as well.

module Main where

import System.Environment (getArgs)
import CompileJS
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Control.Monad
import Control.Applicative ((<$>))

usage = "Usage: kirei <input filename(s)> [output filename]"

main = do
  args <- getArgs
  case args of
    [] -> putStrLn usage
    (fname:_) -> do
      src <- readFile fname
      writeFile (getName fname) (preamble ++ renderJS src)
      putStrLn $ "Wrote output to " ++ getName fname
  where
    getName = splitOn "." ~> init ~> intercalate "." ~> (++ ".js")


