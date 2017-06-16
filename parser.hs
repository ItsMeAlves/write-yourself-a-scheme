module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$ %&|*+ -/: <=? > @^_ ~#"

readExpression :: String -> String
readExpression input = parseResult $ parse symbol "lisp" input
  where parseResult (Left err) = "No match: " ++ show err
        parseResult (Right err) = "Found value"

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpression (args !! 0)
