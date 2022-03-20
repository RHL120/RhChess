module Main where

import Lib
import Visual

import Data.Either (either)
import Data.Maybe (fromMaybe)
import Diagrams.Backend.SVG.CmdLine (B, mainWith)
import Diagrams.Prelude (Diagram)
import System.Exit (exitFailure)
import Text.Read (readEither, readMaybe)

data Config =
  Config
    { boardPath :: FilePath
    , square :: [Square]
    }

getSquare :: IO [Square]
getSquare = do
  sq <- getLine
  case sq of
    str@('(':_) ->
      either
        (\x -> putStrLn "failed to parser input" >> exitFailure)
        (\x -> return [x])
        (readEither sq)
    str@('[':_) ->
      either
        (\x -> putStrLn "failed to parse input" >> exitFailure)
        return
        (readEither sq)
    _ -> [(x, y) | x <- [0 .. 7], y <- [0 .. 7]] <$ putStrLn "Getting all moves"

draw :: String -> [Square] -> Either String (Diagram B)
draw x y = do
  b <- parseBoard x
  moves <- concat <$> traverse (getPossibs b) y
  return (drawBoard b moves)

run :: Config -> IO ()
run (Config bp sq) = do
  f <- readFile bp
  either (\x -> do putStrLn $ "error : " ++ x) mainWith (draw f sq)

main :: IO ()
main = do
  putStrLn "Diagrams extended argumnets are so stupid so I decided to prompt"
  putStrLn "Where is the board:"
  bp <- getLine
  putStrLn "Which square:"
  sq <- getSquare
  run (Config bp sq)
  return ()
