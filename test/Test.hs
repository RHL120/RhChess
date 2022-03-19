module Main where

import Lib
import Test.Hspec

startingString :: String
startingString =
  unlines $
  ["br bn bb bq bk bb bn br", "bp bp bp bp bp bp bp bp"] ++
  replicate 4 "ee ee ee ee ee ee ee ee" ++
  ["wp wp wp wp wp wp wp wp", "wr wn wb wq wk wb wn wr"]

bp :: ChessPieceType -> Piece
bp = Piece Black

wp :: ChessPieceType -> Piece
wp = Piece White

startingBoard :: Board
startingBoard =
  [ [ bp Rook
    , bp Knight
    , bp Bishop
    , bp Queen
    , bp King
    , bp Bishop
    , bp Knight
    , bp Rook
    ]
  , replicate 8 (bp Pawn)
  ] ++
  replicate 4 (replicate 8 Empty) ++
  [ replicate 8 (wp Pawn)
  , [ wp Rook
    , wp Knight
    , wp Bishop
    , wp Queen
    , wp King
    , wp Bishop
    , wp Knight
    , wp Rook
    ]
  ]

main :: IO ()
main =
  hspec $ do
    describe "Parses a board correctly" $ do
      it "can parse the starting board" $ do
        parseBoard startingString `shouldBe` Right startingBoard
