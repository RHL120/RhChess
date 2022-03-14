module Main where

import Lib
import Visual

import Data.Either (either)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

main :: IO ()
main = do
  f <- readFile "./b"
  either putStrLn mainWith (drawBoard <$> parseBoard f)
