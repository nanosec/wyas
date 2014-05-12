module Wyas.Parser (readExprs) where

import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Text.ParserCombinators.Parsec
import Wyas.Types

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
parseList = liftM List parseExprs'

parseDottedList :: Parser LispVal
parseDottedList = do
  x     <- spaces >> parseExpr
  xs    <- manyTill (spaces >> parseExpr) (try $ spaces >> char '.')
  xLast <- spaces >> parseExpr
  spaces >> return (DottedList (x:xs) xLast)

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

parseExprs' :: Parser [LispVal]
parseExprs' = sepBy' parseExpr spaces
    where sepBy' p sep = sep >> endBy p sep

parseExprs :: Parser [LispVal]
parseExprs =
    do x <- parseExprs'
       endOfInput <|> illegalChar
       return x
    where endOfInput = eof >> return []
          illegalChar = anyChar >>= unexpected . show

readBy :: Parser a -> String -> ThrowsError a
readBy p = either (throwError . Parser) return . parse p ""

readExprs :: String -> ThrowsError [LispVal]
readExprs = readBy parseExprs
