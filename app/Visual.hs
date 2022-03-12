module Visual where

import Data.List.Split
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import Lib

data PieceImages =
  PieceImages
    { whitePawnImage :: Diagram B
    , whiteRookImage :: Diagram B
    , whiteKnightImage :: Diagram B
    , whiteBishopImage :: Diagram B
    , whiteQueenImage :: Diagram B
    , whiteKingImage :: Diagram B
    , blackPawnImage :: Diagram B
    , blackRookImage :: Diagram B
    , blackKnightImage :: Diagram B
    , blackBishopImage :: Diagram B
    , blackQueenImage :: Diagram B
    , blackKingImage :: Diagram B
    }

chessSquare :: Colour Double -> Diagram B
chessSquare c = square 1 # lw none # fc c

boardRow :: Int -> Diagram B
boardRow i
  | even i =
    hcat (take 8 $ cycle [chessSquare antiquewhite, chessSquare saddlebrown])
  | otherwise =
    hcat (take 8 $ cycle [chessSquare saddlebrown, chessSquare antiquewhite])

squareToPoint :: Square -> P2 Double
squareToPoint (x, y) = fromIntegral x ^& negate (fromIntegral y)

chessBoard :: Diagram B
chessBoard = vcat (map boardRow [0 .. 7]) # showOrigin
