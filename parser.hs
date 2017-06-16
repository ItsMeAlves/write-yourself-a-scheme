module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispValue =
  Atom String
  | String String
  | Boolean Bool
  | Float Float
  | Number Integer
  | List [LispValue]
  | DottedList [LispValue] LispValue
  deriving (Show)

symbol :: Parser Char
symbol = oneOf "!$ %&|*+ -/: <=? > @^_ ~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispValue
parseString = do
  char '"'
  x <- many $ noneOf "\""
  char '"'
  return $ String x

parseNumber :: Parser LispValue
parseNumber = parseInteger
  <|> parseDigital

parseInteger :: Parser LispValue
parseInteger = (many1 digit) >>= return . Number . read

parseDigital :: Parser LispValue
parseDigital = do
  try $ string "#d"
  num <- many1 digit
  (return . Number . read) num

parseAtom :: Parser LispValue
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  return $ Atom (first : rest)

parseBoolean :: Parser LispValue
parseBoolean = do
  string "#"
  value <- oneOf "tf"
  (return . Boolean) $ booleanIdentification value
  where booleanIdentification 't' = True
        booleanIdentification 'f' = False

parseFloat :: Parser LispValue
parseFloat = do
  left <- many1 digit
  try $ char '.'
  right <- many1 digit
  (return . Float . read) (left ++ "." ++ right)

readExpression :: String -> String
readExpression input = parseResult $ parse parser "lisp" input
  where parseResult (Left err) = "No match: " ++ show err
        parseResult (Right val) = "Found value: " ++ show val
        parser = parseAtom
          <|> parseString
          <|> try parseFloat
          <|> parseNumber
          <|> parseBoolean
        
main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpression (args !! 0)
