module Main where

import Text.Printf (printf)

data PlayerColor
  = Black
  | White
  deriving (Show)

data ChessPieceType
  = Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  deriving (Show)

data Piece
  = Piece
      { pieceColor :: PlayerColor
      , pieceType :: ChessPieceType
      }
  | Empty
  deriving (Show)

type Board = [[Piece]]

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

main :: IO ()
main = do
  f <- readFile "./b"
  print $ parseBoard f
