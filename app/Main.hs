module Main where

import Lib
import Visual

import Data.Either (either)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

boardImage :: String -> [Square] -> Either String (Diagram B)
boardImage str squares = do
  board <- parseBoard str
  possibs <- concat <$> traverse (getPossibs board) squares
  return (drawBoard board possibs)

main :: IO ()
main = do
  f <- readFile "./b"
  either
    putStrLn
    mainWith
    (boardImage f [(x, y) | x <- [0 .. 7], y <- [0 .. 7]])
