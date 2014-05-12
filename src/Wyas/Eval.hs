module Wyas.Eval (evals) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join, liftM)
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (liftIO)
import Wyas.Environment (getVar, setVar, defineVar, bindVars)
import Wyas.Parser (readExprs)
import Wyas.Primitives (elem', liftCheckedIO, readStdin)
import Wyas.Types

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom name) = getVar env name
eval _ val@(Bool   _) = return val
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ (List [Atom "quote", val]) = return val
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
eval env (List [Atom "if", predicate, conseq, alt]) =
    do result <- eval env predicate
       case result of
         Bool False -> eval env alt
         _ -> eval env conseq
eval env form@(List (Atom "cond" : clauseList)) =
    clauseRecur clauseList
    where clauseRecur (List (test : exprs) : rest) =
              case test of
                Atom "else" ->
                    if null rest
                       then evalExprs env exprs
                       else throwError $
                                BadSpecialForm "'else' clause not last" form
                _ -> do result <- eval env test
                        case result of
                          Bool False -> clauseRecur rest
                          _ -> case exprs of
                                 [] -> return result
                                 [Atom "=>", expr] -> eval env expr >>=
                                                      flip apply [result]
                                 _ -> evalExprs env exprs
          clauseRecur [] =
              throwError $ BadSpecialForm "cond: no true clause" form
          clauseRecur _ = throwError $ BadSpecialForm "ill-formed cond" form
eval env form@(List (Atom "case" : key : clauseList)) =
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
                          else throwError $
                                   BadSpecialForm "'else' clause not last" form
                   _ -> throwError $
                            BadSpecialForm "case: not a list or 'else'" test
             clauseRecur [] _ =
                 throwError $ BadSpecialForm "case: no true clause" form
             clauseRecur _ _ =
                 throwError $ BadSpecialForm "ill-formed case" form
eval env form@(List (Atom "define" : rest)) =
    case rest of
      [Atom var, expr] -> eval env expr >>= defineVar env var
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
    liftCheckedIO (readFile filename) >>=
    liftThrows . readExprs >>=
    evalExprs env
eval env (List [Atom "read"]) = readStdin env
eval env (List (function : args)) = join (apply <$> f <*> xs)
    where f = eval env function
          xs = mapM (eval env) args
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evals :: Env -> [LispVal] -> IOThrowsError [LispVal]
evals = mapM . eval

evalExprs :: Env -> [LispVal] -> IOThrowsError LispVal
evalExprs _ [] = throwError NoExprs
evalExprs env exprs = liftM last $ evals env exprs

fromAtom :: LispVal -> ThrowsError String
fromAtom (Atom x) = return x
fromAtom notVar = throwError $ TypeMismatch "variable" notVar

makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> Env ->
            IOThrowsError LispVal
makeFunc vararg params body env =
    do params' <- liftThrows $ mapM fromAtom params
       return $ Func params' vararg body env

makeNormalFunc :: [LispVal] -> [LispVal] -> Env -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVariadicFunc :: LispVal -> [LispVal] -> [LispVal] -> Env ->
                    IOThrowsError LispVal
makeVariadicFunc vararg params body env =
    do vararg' <- liftThrows $ fromAtom vararg
       makeFunc (Just vararg') params body env

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params vararg body closure) args =
    if (numParams > numArgs) || (vararg == Nothing && numParams < numArgs)
       then throwError $ NumArgs (show numParams) args
       else liftIO (bindVars closure argBindings) >>= flip evalExprs body
    where numParams = length params
          numArgs = length args
          (reqArgs, optArgs) = splitAt numParams args
          varargBinding = case vararg of
                            Nothing -> []
                            Just var -> [(var, List optArgs)]
          argBindings = varargBinding ++ zip params reqArgs
apply notFunction _ = throwError $ NotFunction notFunction
