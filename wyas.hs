{-# LANGUAGE ExistentialQuantification #-}

module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Control.Monad
import Control.Monad.Error
import Data.IORef

data LispVal = Atom String
              | Bool Bool
              | String String
              | Number Integer
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
              | Func {params :: [String],
                      vararg :: (Maybe String),
                      body :: [LispVal],
                      closure :: Env}

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> runOne $ args !! 0
            _ -> putStrLn "Program takes only 0 or 1 argument"

--REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

--Parsing

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parsePair :: Parser LispVal
parsePair = do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parsePair
            <|> parseQuoted

--Errors

data LispError = Default String
               | Parser ParseError
               | NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String

showError :: LispError -> String
showError (Default message) = message
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

--Environments

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var =
    readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =
    do env <- liftIO $ readIORef envRef
       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
             (liftIO . readIORef)
             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
    do env <- liftIO $ readIORef envRef
       maybe (throwError $ UnboundVar "Setting an unbound variable" var)
             (liftIO . (flip writeIORef value))
             (lookup var env)
       return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = 
    do alreadyDefined <- liftIO $ isBound envRef var
       if alreadyDefined
          then setVar envRef var value
          else liftIO $ do valueRef <- newIORef value
                           env <- readIORef envRef
                           writeIORef envRef ((var, valueRef) : env)
                           return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

--Evaluation

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "and" : exprs)) =
    case exprs of
      [] -> return $ Bool True
      [expr] -> eval env expr
      (expr : rest) -> do result <- eval env expr
                          case result of
                            Bool False -> return result
                            _ -> eval env (List (Atom "and" : rest))
eval env (List (Atom "or" : exprs)) =
    case exprs of
      [] -> return $ Bool False
      [expr] -> eval env expr
      (expr : rest) -> do result <- eval env expr
                          case result of
                            Bool False -> eval env (List (Atom "or" : rest))
                            _ -> return result
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         _ -> eval env conseq
eval env (List (Atom "cond" : clauseList)) =
    clauseRecur clauseList
    where clauseRecur (List (test : exprs) : rest) =
              case test of
                Atom "else" ->
                    if null rest
                       then liftM last $ mapM (eval env) exprs
                       else throwError $ Default "'else' clause must be last"
                _ -> do result <- eval env test
                        case result of
                          Bool False -> clauseRecur rest
                          _ -> if null exprs
                                  then return result
                                  else liftM last $ mapM (eval env) exprs
eval env (List (Atom "case" : key : clauseList)) =
    do value <- eval env key
       clauseRecur clauseList value
       where clauseRecur (List (test : exprs) : rest) value =
                 case test of
                   List [datum] ->
                       do result <- liftThrows $ eqv [value, datum]
                          case result of
                            Bool False -> clauseRecur rest value
                            _ -> liftM last $ mapM (eval env) exprs
                   Atom "else" ->
                       if null rest
                          then liftM last $ mapM (eval env) exprs
                          else throwError $ Default "'else' clause must be last"
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List (Atom func : args)) =
    mapM (eval env) args >>= liftThrows . apply func
eval env badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
          ($ args)
          (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
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
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) ->
                [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n
                       in if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [] = throwError $ NumArgs 1 []
unaryOp op [param] = return $ op param

symbolPred :: LispVal -> LispVal
symbolPred (Atom _) = Bool True
symbolPred _ = Bool False

stringPred :: LispVal -> LispVal
stringPred (String _) = Bool True
stringPred _ = Bool False

numberPred :: LispVal -> LispVal
numberPred (Number _) = Bool True
numberPred _ = Bool False

symbolToString :: LispVal -> LispVal
symbolToString (Atom s) = String s

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s

boolOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] ->
          ThrowsError LispVal
boolOp _ _ [] = throwError $ NumArgs 2 []
boolOp _ _ arg@[_] = throwError $ NumArgs 2 arg
boolOp unpacker op args = do
  unpackedArgs <- mapM unpacker args
  return . Bool . and $ zipWith op unpackedArgs (tail unpackedArgs)

numBoolOp = boolOp unpackNum
strBoolOp = boolOp unpackStr
boolBoolOp = boolOp unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

eqvalList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvalList eqvalFunc [List arg1, List arg2] =
    return $ Bool $ (length arg1 == length arg2) && (all eqvalPair $ zip arg1 arg2)
    where eqvalPair (x1, x2) = case eqvalFunc [x1, x2] of
                                 Right (Bool val) -> val
                                 Left err -> False

eqvalDotted :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvalDotted eqvalFunc [DottedList xs x, DottedList ys y] =
    do lastEqval <- eqvalFunc [x,y]
       case lastEqval of
         Bool True -> eqvalFunc [List xs, List ys]
         false -> return false

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv arg@([List arg1, List arg2]) = eqvalList eqv arg
eqv arg@([DottedList xs x, DottedList ys y]) = eqvalDotted eqv arg
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    result `catchError` (const $ return False)
    where result = do unpacked1 <- unpacker arg1
                      unpacked2 <- unpacker arg2
                      return $ unpacked1 == unpacked2

equal :: [LispVal] -> ThrowsError LispVal
equal arg@([List arg1, List arg2]) = eqvalList equal arg
equal arg@([DottedList xs x, DottedList ys y]) = eqvalDotted equal arg
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) anyUnpackers
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
  where anyUnpackers = [AnyUnpacker unpackNum,
                        AnyUnpacker unpackStr,
                        AnyUnpacker unpackBool]
equal badArgList = throwError $ NumArgs 2 badArgList

--instance Show

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ argStr ++ varargStr ++ ") ...)"
    where argStr = unwords (map show args)
          varargStr = case varargs of Nothing -> ""
                                      Just arg -> " . " ++ arg

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
