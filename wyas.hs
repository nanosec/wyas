{-# LANGUAGE ExistentialQuantification #-}

module Main where
import Text.ParserCombinators.Parsec
import System.Environment
import System.IO
import GHC.IO.Handle.Types
import Control.Monad
import Control.Monad.Error
import qualified Control.Exception as Ex
import Data.IORef

data LispVal = Atom String
              | Bool Bool
              | String String
              | Number Integer
              | Port Handle
              | EOF
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
              | IOFunc ([LispVal] -> IOThrowsError LispVal)
              | Func {params :: [String],
                      vararg :: (Maybe String),
                      body :: [LispVal],
                      closure :: Env}

main :: IO ()
main = do args <- getArgs
          case args of
            []    -> runRepl
            [arg] -> runFile arg
            _     -> putStrLn "Program takes only 0 or 1 argument"

--REPL

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

promptInput :: String -> IO String
promptInput prompt = flushStr prompt >> getLine

readEval :: Env -> String -> IOThrowsError [LispVal]
readEval env exprs = (liftThrows $ readExprs exprs) >>= evals env

printResults :: IOThrowsError [LispVal] -> IO ()
printResults = (mapM_ putStrLn =<<) . ioThrowsToIOStrings

readEvalPrint :: Env -> String -> IO ()
readEvalPrint env = printResults . readEval env

runFile :: String -> IO ()
runFile filename =
    primitiveBindings >>= flip readEvalPrint ("(load \"" ++ filename ++ "\")")

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>=
          until_ (== "quit") (promptInput "λ> ") . readEvalPrint

--Parsing

readBy :: Parser a -> String -> ThrowsError a
readBy p = either (throwError . Parser) return . parse p ""

readExpr :: String -> ThrowsError LispVal
readExpr = readBy parseExpr

readExprs :: String -> ThrowsError [LispVal]
readExprs = readBy parseExprs

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
parseString = liftM String . between (char '"') (char '"') $ many (noneOf "\"")

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List parseExprs

parseDottedList :: Parser LispVal
parseDottedList = do
  x    <- spaces >> parseExpr
  xs   <- manyTill (spaces >> parseExpr) (try $ spaces >> char '.')
  tail <- spaces >> parseExpr
  spaces >> return (DottedList (x:xs) tail)

parsePair :: Parser LispVal
parsePair = between (char '(') (char ')') $ try parseDottedList <|> parseList

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

sepBy' p sep = sep >> endBy p sep

parseExprs :: Parser [LispVal]
parseExprs = sepBy' parseExpr spaces

--Errors

data LispError = Default String
               | Parser ParseError
               | NumArgs Int [LispVal]
               | TypeMismatch String LispVal
               | BadSpecialForm String LispVal
               | NotFunction LispVal
               | UnboundVar String

showError :: LispError -> String
showError (Default message) = message
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (NumArgs expected found) =
    "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction value) = "Not a function: " ++ show value
showError (UnboundVar var) = "Unbound variable: " ++ var

instance Show LispError where show = showError

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

--Environments

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

actOnVar :: (IORef LispVal -> IOThrowsError LispVal) -> Env -> String ->
            IOThrowsError LispVal
actOnVar f envRef var =
    do env <- liftIO $ readIORef envRef
       maybe (throwError $ UnboundVar var) f (lookup var env)

getVar :: Env -> String -> IOThrowsError LispVal
getVar = actOnVar (liftIO . readIORef)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
    actOnVar f envRef var
    where f valueRef = liftIO $ writeIORef valueRef value >> return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value =
    setVar envRef var value `catchError` makeVar
    where makeVar _ = liftIO $ do valueRef <- newIORef value
                                  modifyIORef envRef ((var, valueRef):)
                                  return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

primitiveBindings :: IO Env
primitiveBindings =
    nullEnv >>= flip bindVars allPrimitives
    where allPrimitives = map (makeValue PrimitiveFunc) primitives
                          ++ map (makeValue IOFunc) ioPrimitives
          makeValue constructor (var, func) = (var, constructor func)

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
eval env form@((List (Atom "cond" : clauseList))) =
    clauseRecur clauseList
    where clauseRecur (List (test : exprs) : rest) =
              case test of
                Atom "else" ->
                    if null rest
                       then evalExprs env exprs
                       else throwError $ Default "'else' clause must be last"
                _ -> do result <- eval env test
                        case result of
                          Bool False -> clauseRecur rest
                          _ -> if null exprs
                                  then return result
                                  else evalExprs env exprs
          clauseRecur [] =
              throwError $ BadSpecialForm "cond: no true clause" form
          clauseRecur _ = throwError $ BadSpecialForm "ill-formed cond" form
eval env form@((List (Atom "case" : key : clauseList))) =
    do value <- eval env key
       clauseRecur clauseList value
       where clauseRecur (List (test : exprs) : rest) value =
                 case test of
                   List datums ->
                       do result <- liftThrows $ value `elem'` datums
                          if result
                             then evalExprs env exprs
                             else clauseRecur rest value
                   Atom "else" ->
                       if null rest
                          then evalExprs env exprs
                          else throwError $ Default "'else' clause must be last"
             clauseRecur [] _ =
                 throwError $ BadSpecialForm "case: no true clause" form
             clauseRecur _ _ =
                 throwError $ BadSpecialForm "ill-formed case" form
eval env form@(List (Atom "define" : rest)) =
    case rest of
      [Atom var, form] -> eval env form >>= defineVar env var
      (List (Atom var : params) : body) ->
          makeNormalFunc params body env >>= defineVar env var
      (DottedList (Atom var : params) vararg : body) ->
          makeVariadicFunc vararg params body env >>= defineVar env var
      _ -> throwError $ BadSpecialForm "ill-formed define" form
eval env form@(List (Atom "lambda" : rest)) =
    case rest of
      (List params : body) -> makeNormalFunc params body env
      (DottedList params vararg : body) ->
          makeVariadicFunc vararg params body env
      (vararg@(Atom _) : body) -> makeVariadicFunc vararg [] body env
      _ -> throwError $ BadSpecialForm "ill-formed lambda" form
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "load", String filename]) =
    (liftIO $ readFile filename) >>= liftThrows . readExprs >>= evalExprs env
eval env (List (function : args)) =
    do f <- eval env function
       xs <- mapM (eval env) args
       apply f xs
eval env badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm

evals :: Env -> [LispVal] -> IOThrowsError [LispVal]
evals = mapM . eval

evalExprs :: Env -> [LispVal] -> IOThrowsError LispVal
evalExprs env = liftM last . evals env

elem' :: LispVal -> [LispVal] -> ThrowsError Bool
elem' _ [] = return False
elem' x (y:ys) = do result <- eqv [x, y]
                    case result of
                      Bool True -> return True
                      _ -> elem' x ys

extract :: LispVal -> String
extract (Atom x) = x

makeFunc :: (Maybe String) -> [LispVal] -> [LispVal] -> Env ->
            IOThrowsError LispVal
makeFunc vararg params body env =
    return $ Func (map extract params) vararg body env

makeNormalFunc = makeFunc Nothing
makeVariadicFunc = makeFunc . Just . extract

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params vararg body closure) args =
    if (numParams > numArgs) || (vararg == Nothing && numParams < numArgs)
       then throwError $ NumArgs numParams args
       else (liftIO $ bindVars closure argBindings) >>= flip evalExprs body
    where numParams = length params
          numArgs = length args
          (reqArgs, optArgs) = splitAt numParams args
          varargBinding = case vararg of
                            Nothing -> []
                            Just var -> [(var, List optArgs)]
          argBindings = varargBinding ++ zip params reqArgs
apply notFunction _ = throwError $ NotFunction notFunction

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericOp (+)),
              ("-", numericOp (-)),
              ("*", numericOp (*)),
              ("/", numericOp div),
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

numericOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericOp _ [] = throwError $ NumArgs 2 []
numericOp _ arg@[_] = throwError $ NumArgs 2 arg
numericOp op args = mapM unpackNum args >>= return . Number . foldl1 op

numericBinop :: (Integer -> Integer -> Integer) ->
                [LispVal] -> ThrowsError LispVal
numericBinop op args@[_,_] = numericOp op args
numericBinop _ args = throwError $ NumArgs 2 args

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n
                       in if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp op [arg] = return $ op arg

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

eqvalList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] ->
             ThrowsError LispVal
eqvalList eqvalFunc [List xs, List ys] =
    return . Bool $ (length xs == length ys) && (all eqvalPair $ zip xs ys)
    where eqvalPair (x, y) = case eqvalFunc [x, y] of
                               Right (Bool val) -> val
                               Left err -> False

eqvalDotted :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] ->
               ThrowsError LispVal
eqvalDotted eqvalFunc [DottedList xs x, DottedList ys y] =
    do lastEqval <- eqvalFunc [x,y]
       case lastEqval of
         Bool True -> eqvalFunc [List xs, List ys]
         false -> return false

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Atom arg1, Atom arg2] = return . Bool $ arg1 == arg2
eqv [Bool arg1, Bool arg2] = return . Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return . Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return . Bool $ arg1 == arg2
eqv [EOF, EOF] = return $ Bool True
eqv arg@([List _, List _]) = eqvalList eqv arg
eqv arg@([DottedList _ _, DottedList _ _]) = eqvalDotted eqv arg
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
equal arg@([List _, List _]) = eqvalList equal arg
equal arg@([DottedList _ _, DottedList _ _]) = eqvalDotted equal arg
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) anyUnpackers
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
  where anyUnpackers = [AnyUnpacker unpackNum,
                        AnyUnpacker unpackStr,
                        AnyUnpacker unpackBool]
equal badArgList = throwError $ NumArgs 2 badArgList

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
    either (\e -> throwError . Default $ show (e :: Ex.IOException)) return

actOnPort :: IOMode -> Handle -> IOThrowsError LispVal -> IOThrowsError LispVal
actOnPort mode port action =
    do portIsXable <- liftCheckedIO $ hIsXable port
       if portIsXable
          then action
          else throwError . TypeMismatch (portType ++ " port") $ Port port
    where (hIsXable, portType) = case mode of
                                   ReadMode -> (hIsReadable, "input")
                                   WriteMode -> (hIsWritable, "output")

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] =
    liftM Port . liftCheckedIO $ openFile filename mode
makePort _ [notString] = throwError $ TypeMismatch "string" notString
makePort _ badArgList = throwError $ NumArgs 1 badArgList

readPort :: [LispVal] -> IOThrowsError LispVal
readPort [] = readPort [Port stdin]
readPort [Port port] = actOnPort ReadMode port action
    where action = do isEOF <- liftCheckedIO $ hIsEOF port
                      if isEOF
                         then return EOF
                         else liftCheckedIO (hGetLine port) >>=
                              liftThrows . readExpr
readPort [notPort] = throwError $ TypeMismatch "input port" notPort
readPort badArgList = throwError $ NumArgs 1 badArgList

writePort :: [LispVal] -> IOThrowsError LispVal
writePort [obj] = writePort [obj, Port stdout]
writePort [obj, Port port] = actOnPort WriteMode port action
    where action = liftCheckedIO $ hPrint port obj >> return (Bool True)
writePort [_, notPort] = throwError $ TypeMismatch "output port" notPort
writePort badArgList = throwError $ NumArgs 2 badArgList

closePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
closePort mode [Port port] =
    do isClosed <- liftCheckedIO $ hIsClosed port
       if isClosed
          then returnValue
          else actOnPort mode port action
    where action = liftCheckedIO (hClose port) >> returnValue
          returnValue = return (Bool True)
closePort _ [notPort] = throwError $ TypeMismatch "port" notPort
closePort _ badArgList = throwError $ NumArgs 1 badArgList

--instance Show

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (Port port) = "<IO port: " ++ filePath ++ ">"
    where filePath = case port of
                       (FileHandle file _) -> file
                       (DuplexHandle file _ _) -> file
showVal EOF = "<eof>"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
    "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (IOFunc _) = "<IO primitive>"
showVal (Func {params = params, vararg = vararg}) =
    "(lambda (" ++ paramStr ++ varargStr ++ ") ...)"
    where paramStr = unwords params
          varargStr = case vararg of Nothing -> ""
                                     Just arg -> " . " ++ arg

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
