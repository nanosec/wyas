module Wyas.Primitives.IO
    (
      ioPrimitives
    , liftCheckedIO
    , stdinPort
    , readStdin
    ) where

import qualified Control.Exception as Ex (try, IOException)
import Control.Monad.Error (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.List (isInfixOf)
import System.IO
import Wyas.Environment (getVar)
import Wyas.Parser (readExprs)
import Wyas.Types

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
