module Main where

import Text.Printf (printf)

data PlayerColor
  = Black
  | White
  deriving (Show, Eq)

data ChessPieceType
  = Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  deriving (Show, Eq)

data Piece
  = Piece
      { pieceColor :: PlayerColor
      , pieceType :: ChessPieceType
      }
  | Empty
  deriving (Show, Eq)

type Board = [[Piece]]

type Square = (Int, Int)

readPieceType :: String -> Either String ChessPieceType
readPieceType str =
  case str of
    "p" -> Right Pawn
    "r" -> Right Rook
    "n" -> Right Knight
    "b" -> Right Bishop
    "q" -> Right Queen
    "k" -> Right King
    _ -> Left $ printf "piece %s is invalid" str

readPiece :: String -> Either String Piece
readPiece str
  | length str < 2 = Left "string length is too low"
  | h == 'b' = Piece Black <$> pt -- piece is black
  | h == 'w' = Piece White <$> pt -- piece is white
  | str == "ee" = Right Empty -- there is no piece here
  | otherwise = Left $ printf "%s has no valid color" str
  where
    h = head str
    pt = readPieceType (tail str)

boardIsFit :: [[a]] -> Bool
boardIsFit l = length l == 8 && all (\x -> length x == 8) l

parseBoard :: String -> Either String Board
parseBoard xs
  | not $ boardIsFit bl = Left "The string does not provied an 8x8 matrix"
  | otherwise = traverse (traverse readPiece) bl
  where
    bl = map words $ lines xs

isValidSquare :: Square -> Bool
isValidSquare (x, y) = x < 8 && y < 8

isFree :: Board -> PlayerColor -> Square -> Either String Bool
isFree b color s@(x, y)
  | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
  | not $ isValidSquare s = Left "Square is not valid"
  | piece == Empty || pieceColor piece /= color = Right True
  | otherwise = Right False
  where
    piece = b !! y !! x

knightPossibs :: Board -> Square -> [Square]
knightPossibs b s@(x, y) =
  [ (x + 1, y + 2)
  , (x - 1, y + 2)
  , (x - 2, y + 1)
  , (x - 2, y - 1)
  , (x - 1, y - 2)
  , (x + 1, y - 2)
  , (x + 2, y - 1)
  , (x + 2, y + 1)
  ]

pawnPossibs :: Board -> PlayerColor -> Square -> [Square]
pawnPossibs b c s@(x, y) =
  if c == Black
    then [(x, y + 1), (x + 1, y + 1), (x - 1, y + 1)] ++ [(x, y + 2) | y == 1]
    else [(x, y - 1), (x + 1, y - 1), (x - 1, y - 1)] ++ [(x, y - 2) | y == 7]

rookPossibs :: Board -> Square -> [Square]
rookPossibs b s@(x, y) =
  [(x + n, y) | n <- [-7 .. 7]] ++ [(x, y + n) | n <- [-7 .. 7]]

bishopPossibs :: Board -> Square -> [Square]
bishopPossibs b s@(x, y) =
  [(x + n, y + n) | n <- [-7 .. 7]] ++ [(x - n, y + n) | n <- [-7 .. 7]]

queenPossibs :: Board -> Square -> [Square]
queenPossibs b s = rookPossibs b s ++ bishopPossibs b s

getPossibs :: Board -> Square -> Either String [Square]
getPossibs b s@(x, y)
  | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
  | not $ isValidSquare s = Left "Square is not valid"
  | piece == Empty = Right []
  | otherwise =
    Right $
    filter
      (\z -> isFree b (pieceColor piece) z == Right True)
      (filter isValidSquare pt)
  where
    piece = b !! y !! x
    pt =
      case pieceType piece of
        Knight -> knightPossibs b s
        Rook -> rookPossibs b s
        Bishop -> bishopPossibs b s
        Pawn -> pawnPossibs b (pieceColor piece) s
        Queen -> queenPossibs b s
        _ -> error "A piece has has no moves please report this as a bug"

main :: IO ()
main = do
  f <- readFile "./b"
  return ()
