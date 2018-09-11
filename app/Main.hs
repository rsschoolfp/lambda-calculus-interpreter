module Main where

import           Prelude          (IO, readFile, fst, ($), putStrLn, show)
import           Lib              (compile)
import           Parser           (runParser, expr)

main :: IO ()
main = do
  line   <- readFile "example.lambda"
  result <- runParser expr line
  putStrLn $ show $ compile $ fst result
