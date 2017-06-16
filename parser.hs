module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispValue =
  Atom String
  | String String
  | Boolean Bool
  | Number Integer
  | List [LispValue]
  | DottedList [LispValue] LispValue
  deriving (Show)

symbol :: Parser Char
symbol = oneOf "!$ %&|*+ -/: <=? > @^_ ~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispValue
parseString = do
  char '"'
  x <- many $ noneOf "\""
  char '"'
  return $ String x

parseNumber :: Parser LispValue
parseNumber = (many1 digit) >>= return . Number . read

parseAtom :: Parser LispValue
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  return $ booleanIdentification (first : rest)
  where booleanIdentification "#t" = Boolean True
        booleanIdentification "#f" = Boolean False
        booleanIdentification s = Atom s

readExpression :: String -> String
readExpression input = parseResult $ parse parser "lisp" input
  where parseResult (Left err) = "No match: " ++ show err
        parseResult (Right val) = "Found value: " ++ show val
        parser = parseAtom <|> parseString <|> parseNumber

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpression (args !! 0)
