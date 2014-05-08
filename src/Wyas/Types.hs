module Wyas.Types where

import Control.Monad.Error
import Data.IORef (IORef)
import GHC.IO.Handle.Types (Handle (FileHandle, DuplexHandle))
import Text.ParserCombinators.Parsec (ParseError)

data LispVal = Atom String
              | Bool Bool
              | String String
              | Number Integer
              | Port {handle :: Handle,
                      buffer :: IORef [LispVal]}
              | EOF
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
              | IOFunc ([LispVal] -> IOThrowsError LispVal)
              | Func {params :: [String],
                      vararg :: Maybe String,
                      body :: [LispVal],
                      closure :: Env}

type Env = IORef [(String, IORef LispVal)]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispVal where
    show (Atom name) = name
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (String str) = "\"" ++ str ++ "\""
    show (Number num) = show num
    show (Port port _) = "<IO port: " ++ filePath ++ ">"
        where filePath = case port of
                           (FileHandle file _) -> file
                           (DuplexHandle file _ _) -> file
    show EOF = "<eof>"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList xs x) =
        "(" ++ unwordsList xs ++ " . " ++ show x ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show (IOFunc _) = "<IO primitive>"
    show (Func [] (Just vararg) _ _) = "(lambda " ++ vararg ++ " ...)"
    show (Func params vararg _ _) =
        "(lambda (" ++ paramStr ++ varargStr ++ ") ...)"
        where paramStr = unwords params
              varargStr = maybe "" (" . " ++) vararg

data LispError = Default String
               | Parser ParseError
               | NoExprs
               | IncompleteExpr String
               | BadSpecialForm String LispVal
               | UnboundVar String
               | NotFunction LispVal
               | NumArgs String [LispVal]
               | TypeMismatch String LispVal
               | DivideByZero
               | IOException String
               | Unexpected String

instance Show LispError where
    show (Default message) = message
    show (Parser parseError) = "Parse error at " ++ show parseError
    show NoExprs = "No expressions to evaluate"
    show (IncompleteExpr line) = "Incomplete expression: " ++ line
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (UnboundVar var) = "Unbound variable: " ++ var
    show (NotFunction value) = "Not a function: " ++ show value
    show (NumArgs range given) =
        "Arity mismatch: expected " ++ range ++ " argument(s), given " ++ values
        where values = case given of [] -> "none"
                                     _  -> "value(s) " ++ unwordsList given
    show (TypeMismatch expected found) =
        "Invalid type: expected " ++ expected ++ ", found " ++ show found
    show DivideByZero = "Error: division by zero"
    show (IOException message) = message
    show (Unexpected wyasFunction) = wyasFunction ++ ": unexpected error"

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

extractAndShow :: ThrowsError [LispVal] -> [String]
extractAndShow = either (\e -> [show e]) (map show)

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = either throwError return

ioThrowsToIOStrings :: IOThrowsError [LispVal] -> IO [String]
ioThrowsToIOStrings = (return . extractAndShow =<<) . runErrorT
