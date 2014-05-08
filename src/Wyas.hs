module Main (main) where

import System.Environment (getArgs)
import Wyas.REPL (runRepl, runFile)

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> runRepl
            _  -> mapM_ runFile args
