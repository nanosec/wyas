module Wyas.Primitives.String (stringPrims) where

import Control.Monad.Error (throwError)
import Wyas.Primitives.Internal (unaryOp, boolOp)
import Wyas.Types

stringPrims :: [(String, [LispVal] -> ThrowsError LispVal)]
stringPrims = [("string=?", strBoolOp (==)),
               ("string<?", strBoolOp (<)),
               ("string>?", strBoolOp (>)),
               ("string<=?", strBoolOp (<=)),
               ("string>=?", strBoolOp (>=)),
               ("symbol->string", unaryOp symbolToString),
               ("string->symbol", unaryOp stringToSymbol)]

fromString :: LispVal -> ThrowsError String
fromString (String str) = return str
fromString notString = throwError $ TypeMismatch "string" notString

strBoolOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolOp = boolOp fromString

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom s) = return $ String s
symbolToString notSymbol = throwError $ TypeMismatch "symbol" notSymbol

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s
stringToSymbol notString = throwError $ TypeMismatch "string" notString
