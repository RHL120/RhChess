module Visual where

import Data.List.Split
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import qualified Lib

chessSquare :: Colour Double -> Diagram B
chessSquare c = square 1 # lw none # fc c

boardRow :: Int -> Diagram B
boardRow i
  | even i =
    hcat (take 8 $ cycle [chessSquare lightgray, chessSquare saddlebrown])
  | otherwise =
    hcat (take 8 $ cycle [chessSquare saddlebrown, chessSquare lightgray])

chessBoard :: Diagram B
chessBoard = vcat (map boardRow [0 .. 7])

squareToPoint :: Lib.Square -> P2 Double
squareToPoint (x, y) = fromIntegral x ^& negate (fromIntegral y)

drawOnSquare :: Diagram B -> Lib.Square -> Diagram B
drawOnSquare d s = d # moveTo (squareToPoint s)

pawnString :: Colour Double -> Diagram B
pawnString c = text "P" # fc c

pieceToString :: Lib.Piece -> Diagram B
pieceToString Lib.Empty = mempty
pieceToString (Lib.Piece c t)
  | t == Lib.Pawn = text "P" # fc color # lc black
  | t == Lib.Rook = text "R" # fc color # lc black
  | t == Lib.Knight = text "N" # fc color # lc black
  | t == Lib.Bishop = text "B" # fc color # lc black
  | t == Lib.Queen = text "Q" # fc color # lc black
  | t == Lib.King = text "K" # fc color # lc black
  | otherwise = mempty
  where
    color =
      if c == Lib.Black
        then black
        else white

drawPiece :: Lib.Piece -> Lib.Square -> Diagram B
drawPiece = drawOnSquare . pieceToString

drawBoard :: Lib.Board -> Diagram B
drawBoard b =
  mconcat ([drawPiece (b !! y !! x) (x, y) | x <- [0 .. 7], y <- [0 .. 7]]) <>
  chessBoard
