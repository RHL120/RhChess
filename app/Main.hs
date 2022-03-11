module Main where

import Debug.Trace (trace)
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

filterBlocked :: Board -> PlayerColor -> [Square] -> Either String [Square]
filterBlocked board color squares
  | not $ boardIsFit board = Left "square is not valid"
  | null squares = Right []
  | otherwise = Right $ fb squares
  where
    fb [] = undefined
    fb (s@(x, y):ss)
      | piece == Empty = s : fb ss
      | pieceColor piece /= color = [s]
      | otherwise = []
      where
        piece = board !! y !! x

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
isValidSquare (x, y) = x < 8 && x >= 0 && y < 8 && y >= 0

isFree :: Board -> PlayerColor -> Square -> Either String Bool
isFree b color s@(x, y)
  | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
  | not $ isValidSquare s = Left "Square is not valid"
  | piece == Empty || pieceColor piece /= color = Right True
  | otherwise = Right False
  where
    piece = b !! y !! x

knightPossibs :: Board -> PlayerColor -> Square -> Either String [Square]
knightPossibs b c s@(x, y)
  | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
  | not $ isValidSquare s = Left "Square is not valid"
  | otherwise =
    Right $
    filter
      (\z -> isValidSquare z && isFree b c z == Right True)
      [ (x + 1, y + 2)
      , (x - 1, y + 2)
      , (x - 2, y + 1)
      , (x - 2, y - 1)
      , (x - 1, y - 2)
      , (x + 1, y - 2)
      , (x + 2, y - 1)
      , (x + 2, y + 1)
      ]

bishopPossibs :: Board -> PlayerColor -> Square -> Either String [Square]
bishopPossibs b c s@(x, y)
  | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
  | not $ isValidSquare s = Left "Square is not valid"
  | otherwise =
    concat <$>
    traverse
      (filterBlocked b c . filter isValidSquare)
      [ [(x - n, y - n) | n <- [1 .. 7]]
      , [(x + n, y + n) | n <- [1 .. 7]]
      , [(x + n, y - n) | n <- [1 .. 7]]
      , [(x - n, y + n) | n <- [1 .. 7]]
      ]

rookPossibs :: Board -> PlayerColor -> Square -> Either String [Square]
rookPossibs b c s@(x, y)
  | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
  | not $ isValidSquare s = Left "Square is not valid"
  | otherwise =
    concat <$>
    traverse
      (filterBlocked b c . filter isValidSquare)
      [ [(x, y + n) | n <- [1 .. 7]]
      , [(x, y - n) | n <- [1 .. 7]]
      , [(x + n, y) | n <- [1 .. 7]]
      , [(x - n, y) | n <- [1 .. 7]]
      ]

pawnPossibs :: Board -> PlayerColor -> Square -> Either String [Square]
pawnPossibs b c s@(x, y)
  | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
  | not $ isValidSquare s = Left "Invalid square"
  | otherwise =
    Right $ filter (\z -> isValidSquare z && isFree b c z == Right True) ps
  where
    ps =
      if c == Black
        then [(x, y + 1), (x + 1, y + 1), (x - 1, y + 1)] ++
             [(x, y + 2) | y == 1]
        else [(x, y - 1), (x + 1, y - 1), (x - 1, y - 1)] ++
             [(x, y - 2) | y == 7]

getPossibs :: Board -> Square -> Either String [Square]
getPossibs b s@(x, y)
  | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
  | not $ isValidSquare s = Left "Square is not valid"
  | piece == Empty = Right []
  | pt == Rook = rookPossibs b c s
  | pt == Bishop = bishopPossibs b c s
  | pt == Knight = knightPossibs b c s
  | pt == Pawn = pawnPossibs b c s
  | pt == Queen = do
    bi <- bishopPossibs b c s
    r <- rookPossibs b c s
    return (bi ++ r)
  | otherwise = error "Unknow piece please report this as a bug"
  where
    piece = b !! y !! x
    pt = pieceType piece
    c = pieceColor piece

main :: IO ()
main = do
  f <- readFile "./b"
  print ((\x -> getPossibs x (2, 0)) =<< parseBoard f)
