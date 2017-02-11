module Main where

import Lambdifies.Evaluator
import Lambdifies.Parser


main :: IO ()
main = do
  ei <- getLine
  run ei


run :: String -> IO ()
run s =
  do
    ep <- parseExpr s
    ee <- initialEval ep
    putStrLn . show $ ee
    putStrLn "Exit."
