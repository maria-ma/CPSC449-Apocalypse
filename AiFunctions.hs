module AiFunctions where

import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import ApocTools
import System.Random

---AI Movement functions-----------------------------------------------------------

-- | placePawn: for ai strategies when placing pawn
--   finds the nearest empty space on the board and sets it to the pawn's destination
placePawn :: Board -> Player -> Int -> Int -> (Int, Int)
placePawn board player toX 5   = placePawn board player (toX+1) 0
placePawn board player toX toY = if (getFromBoard board (toX, toY) == E) then (toX, toY) else placePawn board player toX (toY+1)

-- | getPieces: gets list of playable pieces based on the player
--   params: the board, the player type
--   returns: list of tupbles indicating playable start sources
getPieces :: Board -> Player -> PlayType -> [(Int, Int)]
getPieces board White Normal = getBoardPieces (\x -> x == WP || x == WK) board 0
getPieces board White PawnPlacement = getBoardPieces (\x -> x == WP) board 0
getPieces board Black Normal = getBoardPieces (\x -> x == BP || x == BK) board 0
getPieces board Black PawnPlacement = getBoardPieces (\x -> x == BP) board 0

getEmpty :: Board -> [(Int, Int)]
getEmpty board = getBoardPieces (\x -> x == E) board 0

-- | getBoardPieces: gets all available board pieces
--   input: boolean function indicating which board piece is playable (white or black pieces), the board, starting row number
--   returns: list of tuples of playable pieces
getBoardPieces :: (a -> Bool) -> [[a]] -> Int -> [(Int, Int)]
getBoardPieces func [] _ = []
getBoardPieces func (x:xs) row = (map (\x -> (x, row)) $ getRowPieces func x 0) ++ (getBoardPieces func xs (row + 1))

-- | getRowPieces: gets all playable pieces in a specific row (traverses through the column)
--   input: boolean function called from getBoardPieces, cell from board, (int) current column number
getRowPieces :: (a -> Bool) -> [a] -> Int -> [Int]
getRowPieces func [] _ = []
getRowPieces func (x:xs) column = if func x then column:(getRowPieces func xs (column + 1)) else getRowPieces func xs (column + 1)

-- | getMoveList: gets the list of valid moves based on the current player's piece
getMoveList :: Board -> Player -> (Int, Int) -> [(Int, Int)]
getMoveList board player (fromX, fromY)
    -- * player is currently playing a pawn
    | (startPiece == WP) || (startPiece == BP) = 
        [(fromX, fromY + forward), (fromX - 1, fromY + forward), (fromX + 1, fromY + forward)]
    -- * otherwise, player is currently playing a knight
    | otherwise = 
        [(fromX + 1, fromY + 2), (fromX + 1, fromY - 2), (fromX - 1, fromY - 2), (fromX -1, fromY +2), (fromX + 2, fromY + 1), (fromX + 2, fromY - 1), (fromX -2, fromY - 1), (fromX - 2, fromY + 1)]
    where startPiece = getFromBoard board (fromX, fromY)
          forward = case player of
                        Black -> -1
                        White -> 1

findPawns :: Board -> Player -> Bool
findPawns board player = (length $ getPieces board player PawnPlacement) > 0