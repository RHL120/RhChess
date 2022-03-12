module Main where

import Lib
import Visual

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith chessBoard
