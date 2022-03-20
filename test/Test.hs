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

--If the are the same return Nothign otherwise return Just result
moveChecker :: Piece -> Square -> [Square] -> [Move] -> Maybe [Move]
moveChecker piece src expected result =
  let moves = map (\x -> Move piece x src) expected
   --check if they have the same elements but maybe not in the same order
   in if filter (`elem` result) moves == moves &&
         filter (`elem` moves) result == result && length result == length moves
        then Nothing
        else Just result

rookBoard :: Board
rookBoard =
  [replicate 8 Empty, replicate 3 Empty ++ [bp Pawn] ++ replicate 4 Empty] ++
  replicate 2 (replicate 8 Empty) ++
  [replicate 3 Empty ++ [bp Rook] ++ replicate 4 Empty] ++
  replicate 2 (replicate 8 Empty) ++
  [replicate 3 Empty ++ [wp Pawn] ++ replicate 4 Empty]

rookMoves :: [Square]
rookMoves =
  [ (3, 5)
  , (3, 6)
  , (3, 7)
  , (3, 3)
  , (3, 2)
  , (4, 4)
  , (5, 4)
  , (6, 4)
  , (7, 4)
  , (2, 4)
  , (1, 4)
  , (0, 4)
  ]

bishopBoard :: Board
bishopBoard =
  [ replicate 8 Empty
  , Empty : bp Pawn : replicate 6 Empty
  , replicate 8 Empty
  , replicate 8 Empty
  , replicate 4 Empty ++ bp Bishop : replicate 3 Empty
  , replicate 8 Empty
  , replicate 7 Empty ++ [wp Pawn]
  , replicate 8 Empty
  ]

bishopMoves :: [Square]
bishopMoves =
  [ (3, 3)
  , (2, 2)
  , (5, 5)
  , (6, 6)
  , (7, 7)
  , (5, 3)
  , (6, 2)
  , (7, 1)
  , (3, 5)
  , (2, 6)
  , (1, 7)
  ]

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
      it "correctly gets a rook's moves" $ do
        moveChecker (bp Rook) (3, 4) rookMoves <$>
          getPossibs rookBoard (3, 4) `shouldBe` Right Nothing
      it "correctly gets a bishop's moves" $ do
        moveChecker (bp Bishop) (4, 4) bishopMoves <$>
          getPossibs bishopBoard (4, 4) `shouldBe` Right Nothing
      describe "It parses pawn moves correctly" $ do
        it "correctly gets black's initial moves" $ do
          moveChecker (bp Pawn) (0, 1) [(0, 2), (0, 3)] <$>
            getPossibs pawnStart (0, 1) `shouldBe` Right Nothing
        it "correctly get white's initial moves" $ do
          moveChecker (wp Pawn) (0, 6) [(0, 5), (0, 4)] <$>
            getPossibs pawnStart (0, 6) `shouldBe` Right Nothing
        it "correctly gets black's attack moves" $ do
          moveChecker (bp Pawn) (3, 6) [(2, 7), (4, 7)] <$>
            getPossibs pawnAttack (3, 6) `shouldBe` Right Nothing
        it "correctly gets white's attack moves" $ do
          moveChecker (wp Pawn) (3, 7) [(2, 6), (4, 6)] <$>
            getPossibs pawnAttack (3, 7) `shouldBe` Right Nothing
