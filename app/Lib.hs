module Lib
  ( Square
  , PlayerColor(Black, White)
  , Piece(Piece, Empty)
  , ChessPieceType(Pawn, Rook, Knight, Bishop, Queen, King)
  , Move(Move)
  , Board
  , parseBoard
  , getPossibs
  , boardPossibs
  ) where

import Control.Monad (join)
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

data Move =
  Move
    { movePiece :: Piece
    , moveDst :: Square
    , moveSrc :: Square
    }
  deriving (Show, Eq)

checkInput ::
     (Board -> Square -> Either String b)
  -> (Board -> Square -> Either String b)
checkInput f = g
  where
    g b s
      | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
      | not $ isValidSquare s = Left "Square is not valid"
      | otherwise = f b s

filterBlocked :: Board -> PlayerColor -> [Square] -> Either String [Square]
filterBlocked board color squares
  | not $ boardIsFit board = Left "square is not valid"
  | null squares = Right []
  | otherwise = Right $ fb squares
  where
    fb [] = []
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
isFree board color = checkInput f board
  where
    f b (x, y)
      | piece == Empty || pieceColor piece /= color = Right True
      | otherwise = Right False
      where
        piece = b !! y !! x

knightPossibs :: Board -> PlayerColor -> Square -> Either String [Square]
knightPossibs board c = checkInput f board
  where
    f b (x, y) =
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
bishopPossibs board c = checkInput f board
  where
    f b (x, y) =
      concat <$>
      traverse
        (filterBlocked b c . filter isValidSquare)
        [ [(x - n, y - n) | n <- [1 .. 7]]
        , [(x + n, y + n) | n <- [1 .. 7]]
        , [(x + n, y - n) | n <- [1 .. 7]]
        , [(x - n, y + n) | n <- [1 .. 7]]
        ]

rookPossibs :: Board -> PlayerColor -> Square -> Either String [Square]
rookPossibs board c = checkInput f board
  where
    f b (x, y) =
      concat <$>
      traverse
        (filterBlocked b c . filter isValidSquare)
        [ [(x, y + n) | n <- [1 .. 7]]
        , [(x, y - n) | n <- [1 .. 7]]
        , [(x + n, y) | n <- [1 .. 7]]
        , [(x - n, y) | n <- [1 .. 7]]
        ]

pawnPossibs :: Board -> PlayerColor -> Square -> Either String [Square]
pawnPossibs board c = checkInput f board
  where
    f b s = Right $ apm b c s ++ pam b c s
    apm b c s@(x, y)
      | c == Black && y == 1 = takeWhile checker [(x, y + 1), (x, y + 2)]
      | c == Black = filter checker [(x, y + 1)]
      | c == White && y == 6 = takeWhile checker [(x, y - 1), (x, y - 2)]
      | c == White = filter checker [(x, y - 1)]
      | otherwise = undefined
      where
        checker s@(nx, ny) = isValidSquare s && board !! ny !! nx == Empty
    pam b c s@(x, y)
      | c == Black = filter checker [(x - 1, y + 1), (x + 1, y + 1)]
      | otherwise = filter checker [(x - 1, y - 1), (x + 1, y - 1)]
      where
        checker s@(nx, ny) = isValidSquare s && board !! ny !! nx /= Empty

kingPossibs :: Board -> PlayerColor -> Square -> Either String [Square]
kingPossibs board c = checkInput f board
  where
    f b s@(x, y) =
      Right $
      filter
        (\z -> isValidSquare z && isFree b c z == Right True)
        [ (x + 1, y)
        , (x - 1, y)
        , (x + 1, y + 1)
        , (x + 1, y - 1)
        , (x, y - 1)
        , (x, y + 1)
        , (x - 1, y + 1)
        , (x - 1, y - 1)
        ]

getPossibs :: Board -> Square -> Either String [Move]
getPossibs b s@(x, y)
  | not $ boardIsFit b = Left "Board is not an 8x8 matrix"
  | not $ isValidSquare s = Left "Square is not valid"
  | piece == Empty = Right []
  | otherwise =
    case pt of
      Rook -> toMove $ rookPossibs b c s
      Bishop -> toMove $ bishopPossibs b c s
      Knight -> toMove $ knightPossibs b c s
      Pawn -> toMove $ pawnPossibs b c s
      King -> toMove $ kingPossibs b c s
      Queen -> do
        bi <- bishopPossibs b c s
        r <- rookPossibs b c s
        return $ map (\z -> Move piece z s) (bi ++ r)
  where
    piece = b !! y !! x
    pt = pieceType piece
    c = pieceColor piece
    toMove = join . Right . fmap (map (\z -> Move piece z s))

boardPossibs :: Board -> PlayerColor -> Either String [Move]
boardPossibs b c =
  filter (\(Move x _ _) -> pieceColor x == c) . concat <$> -- no need to for empty here
  traverse (getPossibs b) [(x, y) | x <- [0 .. 7], y <- [1 .. 7]]
