module Wyas.Primitives
    (
      primitives
    , elem'
    , ioPrimitives
    , liftCheckedIO
    , stdinPort
    , readStdin
    ) where

import qualified Control.Exception as Ex (try, IOException)
import Control.Monad.Error (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (foldl1', isInfixOf)
import System.IO
import Wyas.Environment (getVar)
import Wyas.Parser (readExprs)
import Wyas.Types

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericOp (+)),
              ("-", numericOp (-)),
              ("*", numericOp (*)),
              ("/", checkedDiv),
              ("modulo", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolPred),
              ("string?", unaryOp stringPred),
              ("number?", unaryOp numberPred),
              ("symbol->string", unaryOp symbolToString),
              ("string->symbol", unaryOp stringToSymbol),
              ("=", numBoolOp (==)),
              ("<", numBoolOp (<)),
              (">", numBoolOp (>)),
              ("/=", numBoolOp (/=)),
              (">=", numBoolOp (>=)),
              ("<=", numBoolOp (<=)),
              ("string=?", strBoolOp (==)),
              ("string<?", strBoolOp (<)),
              ("string>?", strBoolOp (>)),
              ("string<=?", strBoolOp (<=)),
              ("string>=?", strBoolOp (>=)),
              ("cons", cons),
              ("car", car),
              ("cdr", cdr),
              ("equal?", equal)]

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

elem' :: LispVal -> [LispVal] -> ThrowsError Bool
elem' _ [] = return False
elem' x (y:ys) = do result <- equal [x, y]
                    case result of
                      Bool True -> return True
                      _ -> elem' x ys

checkedDiv :: [LispVal] -> ThrowsError LispVal
checkedDiv args = do zeroExists <- Number 0 `elem'` args
                     if zeroExists
                        then throwError DivideByZero
                        else numericOp div args

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [arg] = op arg
unaryOp _ args = throwError $ NumArgs "1" args

symbolPred :: LispVal -> ThrowsError LispVal
symbolPred (Atom _) = return $ Bool True
symbolPred _        = return $ Bool False

stringPred :: LispVal -> ThrowsError LispVal
stringPred (String _) = return $ Bool True
stringPred _          = return $ Bool False

numberPred :: LispVal -> ThrowsError LispVal
numberPred (Number _) = return $ Bool True
numberPred _          = return $ Bool False

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom s) = return $ String s
symbolToString notSymbol = throwError $ TypeMismatch "symbol" notSymbol

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s
stringToSymbol notString = throwError $ TypeMismatch "string" notString

boolOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] ->
          ThrowsError LispVal
boolOp extractor op args@(_:_:_) =
    do args' <- mapM extractor args
       return . Bool . and . zipWith op args' $ tail args'
boolOp _ _ arg = throwError $ NumArgs "> 1" arg

numBoolOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolOp = boolOp fromNumber

strBoolOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolOp = boolOp fromString

fromString :: LispVal -> ThrowsError String
fromString (String str) = return str
fromString notString = throwError $ TypeMismatch "string" notString

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

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("read", readPort),
                ("write", writePort),
                ("close-input-port", closePort ReadMode),
                ("close-output-port", closePort WriteMode)]

liftCheckedIO :: IO a -> IOThrowsError a
liftCheckedIO action =
    liftIO (Ex.try action) >>=
    either (\e -> throwError . IOException $ show (e :: Ex.IOException)) return

actOnPort :: IOMode -> Handle -> IOThrowsError a -> IOThrowsError a
actOnPort mode port action =
    do portIsXable <- liftCheckedIO $ hIsXable port
       if portIsXable
          then action
          else throwError $ TypeMismatch (portType ++ " port")
                                         (Port port undefined)
    where (hIsXable, portType) = case mode of
                                   ReadMode -> (hIsReadable, "input")
                                   WriteMode -> (hIsWritable, "output")
                                   _ -> error "actOnPort: unexpected error"

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] =
    do port <- liftCheckedIO $ openFile filename mode
       buffer <- liftIO $ newIORef []
       return $ Port port buffer
makePort _ [notString] = throwError $ TypeMismatch "string" notString
makePort _ badArgList = throwError $ NumArgs "1" badArgList

stdinVar :: String
stdinVar = "#stdin"

stdinPort :: IO (String, LispVal)
stdinPort = do buffer <- newIORef []
               return (stdinVar, Port stdin buffer)

readStdin :: Env -> IOThrowsError LispVal
readStdin env = getVar env stdinVar >>= readPort . (:[])

readPort :: [LispVal] -> IOThrowsError LispVal
readPort [Port port buffer] =
    do lispVals <- liftIO $ readIORef buffer
       case lispVals of
         [] -> actOnPort ReadMode port readLine >>=
               liftIO . writeIORef buffer >>
               readPort [Port port buffer]
         x:xs -> liftIO $ writeIORef buffer xs >>
                          return x
    where ifEOF conseq alt = do isEndOfFile <- liftCheckedIO $ hIsEOF port
                                if isEndOfFile then conseq else alt
          readLine = ifEOF (return [EOF])
                           (do line <- liftCheckedIO $ hGetLine port
                               readCompleteExprs line line)
          readCompleteExprs line1 errorLine =
              liftThrows (readExprs line1) `catchError` maybeReadMore
              where maybeReadMore err@(Parser e) =
                        if "unexpected end of input" `isInfixOf` show e
                           then ifEOF (throwError $ IncompleteExpr errorLine)
                                      (do line2 <- liftCheckedIO $ hGetLine port
                                          readCompleteExprs (line1 ++ line2)
                                                            errorLine)
                           else throwError err
                    maybeReadMore _ = throwError $ Unexpected "readPort"
readPort [notPort] = throwError $ TypeMismatch "input port" notPort
readPort badArgList = throwError $ NumArgs "0 or 1" badArgList

writePort :: [LispVal] -> IOThrowsError LispVal
writePort [obj] = writePort [obj, Port stdout undefined]
writePort [obj, Port port _] = actOnPort WriteMode port action
    where action = liftCheckedIO $ hPrint port obj >> return (Bool True)
writePort [_, notPort] = throwError $ TypeMismatch "output port" notPort
writePort badArgList = throwError $ NumArgs "1 or 2" badArgList

closePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
closePort mode [Port port _] =
    do isClosed <- liftCheckedIO $ hIsClosed port
       if isClosed
          then returnValue
          else actOnPort mode port action
    where action = liftCheckedIO (hClose port) >> returnValue
          returnValue = return (Bool True)
closePort _ [notPort] = throwError $ TypeMismatch "port" notPort
closePort _ badArgList = throwError $ NumArgs "1" badArgList
