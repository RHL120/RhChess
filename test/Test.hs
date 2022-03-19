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

pawnStart :: Board
pawnStart =
  [replicate 8 Empty, replicate 8 $ bp Pawn] ++
  replicate 4 (replicate 8 Empty) ++ [replicate 8 $ wp Pawn, replicate 8 Empty]

pawnAttack :: Board
pawnAttack =
  replicate 6 (replicate 8 Empty) ++
  [replicate 8 $ bp Pawn, replicate 8 $ wp Pawn]

main :: IO ()
main =
  hspec $ do
    describe "Parses a board correctly" $ do
      it "can parse the starting board" $ do
        parseBoard startingString `shouldBe` Right startingBoard
      it "fails if the board is not an 8x8 matrix" $ do
        parseBoard "" `shouldBe`
          Left "The string does not provied an 8x8 matrix"
      it "fails if the piece has an invalid color" $ do
        parseBoard (unlines (replicate 8 $ concat $ "fp" : replicate 7 " ee")) `shouldBe`
          Left "fp has no valid color"
      it "fails if the piece has an invalid type" $ do
        parseBoard (unlines (replicate 8 $ concat $ "bf" : replicate 7 " ee")) `shouldBe`
          Left "piece f is invalid"
    describe "Gets moves correctly" $ do
      describe "It parses pawn moves correctly" $ do
        it "correctly gets black's initial moves" $ do
          getPossibs pawnStart (0, 1) `shouldBe`
            Right [Move (bp Pawn) (0, 2) (0, 1), Move (bp Pawn) (0, 3) (0, 1)]
        it "correctly get white's initial moves" $ do
          getPossibs pawnStart (0, 6) `shouldBe`
            Right [Move (wp Pawn) (0, 5) (0, 6), Move (wp Pawn) (0, 4) (0, 6)]
        it "correctly gets black's attack moves" $ do
          getPossibs pawnAttack (3, 6) `shouldBe`
            Right [Move (bp Pawn) (2, 7) (3, 6), Move (bp Pawn) (4, 7) (3, 6)]
        it "correctly gets white's attack moves" $ do
          getPossibs pawnAttack (3, 7) `shouldBe`
            Right [Move (wp Pawn) (2, 6) (3, 7), Move (wp Pawn) (4, 6) (3, 7)]
