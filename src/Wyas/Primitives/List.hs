module Wyas.Primitives.List (listPrims) where

import Control.Monad.Error (throwError)
import Wyas.Types

listPrims :: [(String, [LispVal] -> ThrowsError LispVal)]
listPrims = [("cons", cons),
             ("car", car),
             ("cdr", cdr)]

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs "2" badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "non-empty list" badArg
car badArgList = throwError $ NumArgs "1" badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "non-empty list" badArg
cdr badArgList = throwError $ NumArgs "1" badArgList
