module Wyas.Environment
    (
      nullEnv
    , bindVar
    , bindVars
    , getVar
    , setVar
    , defineVar
    ) where

import Control.Monad (liftM)
import Control.Monad.Error (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Wyas.Types

nullEnv :: IO Env
nullEnv = newIORef []

bindVar :: Env -> (String, LispVal) -> IO Env
bindVar envRef assoc =
    do binding <- makeBinding assoc
       modifyIORef envRef (binding:)
       return envRef
    where makeBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef = liftM last . mapM (bindVar envRef)

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
