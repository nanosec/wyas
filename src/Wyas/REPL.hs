module Wyas.REPL (runFile, runRepl) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join)
import System.IO (hFlush, stdout)
import Wyas.Environment (nullEnv, bindVar, bindVars)
import Wyas.Eval (evals)
import Wyas.Parser (readExprs)
import Wyas.Primitives (stdinPort, primitives, ioPrimitives)
import Wyas.Types

readEval :: Env -> String -> IOThrowsError [LispVal]
readEval env exprs = liftThrows (readExprs exprs) >>= evals env

printResults :: IOThrowsError [LispVal] -> IO ()
printResults = (mapM_ putStrLn =<<) . ioThrowsToIOStrings

readEvalPrint :: Env -> String -> IO ()
readEvalPrint env = printResults . readEval env

primitiveBindings :: IO Env
primitiveBindings =
    join (bindVar <$> nullEnv <*> stdinPort) >>=
    flip bindVars allPrimitives
    where allPrimitives = map (makeValue PrimitiveFunc) primitives
                          ++ map (makeValue IOFunc) ioPrimitives
          makeValue constructor (var, func) = (var, constructor func)

runFile :: String -> IO ()
runFile filename =
    primitiveBindings >>= flip readEvalPrint ("(load \"" ++ filename ++ "\")")

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
     then return ()
     else action result >> until_ predicate prompt action

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

promptInput :: String -> IO String
promptInput prompt = flushStr prompt >> getLine

runRepl :: IO ()
runRepl = primitiveBindings >>=
          until_ (== "quit") (promptInput "λ> ") . readEvalPrint
