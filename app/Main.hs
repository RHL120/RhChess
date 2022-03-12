module Main where

import Lib

main :: IO ()
main = do
  f <- readFile "./b"
  print ((\x -> getPossibs x (5, 7)) =<< parseBoard f)
