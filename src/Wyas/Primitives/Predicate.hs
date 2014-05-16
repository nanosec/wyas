module Wyas.Primitives.Predicate (predicates, elem') where

import Control.Monad.Error (throwError)
import Wyas.Primitives.Internal (unaryOp)
import Wyas.Types

predicates :: [(String, [LispVal] -> ThrowsError LispVal)]
predicates = [("equal?", equal),
              ("symbol?", unaryOp symbolPred),
              ("string?", unaryOp stringPred),
              ("number?", unaryOp numberPred)]

equal :: [LispVal] -> ThrowsError LispVal
equal [Atom   x, Atom   y] = return . Bool $ x == y
equal [Bool   x, Bool   y] = return . Bool $ x == y
equal [String x, String y] = return . Bool $ x == y
equal [Number x, Number y] = return . Bool $ x == y
equal [EOF     , EOF     ] = return $ Bool True
equal [List xs, List ys] =
    return . Bool $ (length xs == length ys) && (all equalPair $ zip xs ys)
    where equalPair (x, y) = case equal [x, y] of
                               Right (Bool val) -> val
                               _ -> error "equal: unexpected error"
equal [DottedList xs x, DottedList ys y] =
    do lastEqual <- equal [x,y]
       case lastEqual of
         Bool True -> equal [List xs, List ys]
         false -> return false
equal [_, _] = return $ Bool False
equal badArgList = throwError $ NumArgs "2" badArgList

elem' :: LispVal -> [LispVal] -> ThrowsError Bool
elem' _ [] = return False
elem' x (y:ys) = do result <- equal [x, y]
                    case result of
                      Bool True -> return True
                      _ -> elem' x ys

symbolPred :: LispVal -> ThrowsError LispVal
symbolPred (Atom _) = return $ Bool True
symbolPred _        = return $ Bool False

stringPred :: LispVal -> ThrowsError LispVal
stringPred (String _) = return $ Bool True
stringPred _          = return $ Bool False

numberPred :: LispVal -> ThrowsError LispVal
numberPred (Number _) = return $ Bool True
numberPred _          = return $ Bool False
