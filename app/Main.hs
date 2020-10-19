module Main where

import Lib

main :: IO ()
main = do
  let state = AppState "100" "0"
      calculated = calc
      result = runReader (runExceptT calculated) state
  putStrLn $ "result: " ++ (show result)
