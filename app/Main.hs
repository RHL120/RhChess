module Main where

import Lib
import Visual

import Data.Either (either)
import Diagrams.Backend.SVG.CmdLine (B, mainWith)
import Diagrams.Prelude (Diagram)
import System.Exit (exitFailure)
import Text.Read (readEither)

data Config =
  Config
    { boardPath :: FilePath
    , square :: Square
    }

-- TODO: Use Maybe
-- TODO: Allow multiple squares
getSquare :: IO Square
getSquare = (read :: String -> Square) <$> getLine

draw :: String -> Square -> Either String (Diagram B)
draw x y = do
  b <- parseBoard x
  moves <- getPossibs b y
  return (drawBoard b moves)

run :: Config -> IO ()
run (Config bp sq) = do
  f <- readFile bp
  either (\x -> do putStrLn $ "error : " ++ x) mainWith (draw f sq)

startingString :: String
startingString =
  unlines $
  ["br bn bb bq bk bb bn br", "bp bp bp bp bp bp bp bp"] ++
  replicate 4 "ee ee ee ee ee ee ee ee" ++
  ["wp wp wp wp wp wp wp wp", "wr wn wb wq wk wb wn wr"]

main :: IO ()
main = do
  print (parseBoard startingString)
  putStrLn "Diagrams extended argumnets are so stupid so I decided to prompt"
  putStrLn "Where is the board:"
  bp <- getLine
  putStrLn "Which square:"
  sq <- getSquare
  run (Config bp sq)
  return ()
