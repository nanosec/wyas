module Wyas.Primitives.Number (numberPrims) where

import Control.Monad.Error (throwError)
import Data.List (foldl1')
import Wyas.Primitives.Internal (boolOp)
import Wyas.Primitives.Predicate (elem')
import Wyas.Types

numberPrims :: [(String, [LispVal] -> ThrowsError LispVal)]
numberPrims = [("+", numericOp (+)),
               ("-", numericOp (-)),
               ("*", numericOp (*)),
               ("/", checkedDiv),
               ("modulo", numericBinop mod),
               ("quotient", numericBinop quot),
               ("remainder", numericBinop rem),
               ("=", numBoolOp (==)),
               ("<", numBoolOp (<)),
               (">", numBoolOp (>)),
               ("/=", numBoolOp (/=)),
               (">=", numBoolOp (>=)),
               ("<=", numBoolOp (<=))]

fromNumber :: LispVal -> ThrowsError Integer
fromNumber (Number num) = return num
fromNumber notNum = throwError $ TypeMismatch "number" notNum

numericOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp op args@(_:_:_) = mapM fromNumber args >>=
                            return . Number . foldl1' op
numericOp _ args = throwError $ NumArgs "> 1" args

numericBinop :: (Integer -> Integer -> Integer) ->
                [LispVal] -> ThrowsError LispVal
numericBinop op args@[_,_] = numericOp op args
numericBinop _ args = throwError $ NumArgs "2" args

checkedDiv :: [LispVal] -> ThrowsError LispVal
checkedDiv args = do zeroExists <- Number 0 `elem'` args
                     if zeroExists
                        then throwError DivideByZero
                        else numericOp div args

numBoolOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolOp = boolOp fromNumber
