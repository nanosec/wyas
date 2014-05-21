module Wyas.Primitives.Internal where

import Control.Monad.Error (throwError)
import Wyas.Types

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [arg] = op arg
unaryOp _ args = throwError $ NumArgs "1" args

binaryOp :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
binaryOp op args@[_,_] = op args
binaryOp _ args = throwError $ NumArgs "2" args

boolOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] ->
          ThrowsError LispVal
boolOp extractor op args@(_:_:_) =
    do args' <- mapM extractor args
       return . Bool . and . zipWith op args' $ tail args'
boolOp _ _ arg = throwError $ NumArgs "> 1" arg
