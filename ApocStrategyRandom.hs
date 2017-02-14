module ApocStrategyRandom  where

import Data.Maybe (fromJust, isNothing)
import Data.Char
import Data.List
import System.Environment
import ApocTools
import System.Random

random :: Chooser
random gamestate Normal player        = return (Just [(2,2)]) --playRandomNormal (theBoard gamestate) player 
random gamestate PawnPlacement player = return (Just [(2,2)])

-- to do: get list of available moves
-- choose randomly
-- or we can choose a random piece from the player
-- randomly generate legal coordinates to go to
-- then if the move if legal go make the move
-- otherwise generate another move

-- intersect board w WK & WP

---AI Movement functions-----------------------------------------------------------

-- | playRandomNormal
--playRandomNormal :: Board -> Player
--playRandomNormal board player

-- | updatePieces
updatePieces ::(Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
updatePieces from to origBoard = replace' origBoard index to
                                     where index = case (elemIndex from origBoard) of
                                                       Just n -> n
                                                       Nothing -> -1

-- | getRandPiece gets random piece from the own pieces board
getRandPiece :: [a] -> IO a
getRandPiece list = do 
    index <- randomRIO (0, (length list)-1)
    return (list !! index)           

replace' :: [a] -> Int -> a -> [a]
replace' xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                         ++ [elem]
                         ++ (if null zs then [] else tail zs)

-- | placePawn: for ai strategies when placing pawn
--   finds the nearest empty space on the board and sets it to the pawn's destination
placePawn :: Board -> Player -> Int -> Int -> (Int, Int)
placePawn board player toX 5   = placePawn board player toX 0
placePawn board player toX toY = if (getFromBoard board (toX, toY) == E) then (toX, toY) else placePawn board player toX (toY+1)

-- | getPieces: gets list of playable pieces based on the player
--   params: the board, the player type
--   returns: list of tupbles indicating playable start sources
getPieces :: Board -> Player -> [(Int, Int)]
getPieces board White = getBoardPieces (\x -> x == WP || x == WK) board 0
getPieces board Black = getBoardPieces (\x -> x == BP || x == BK) board 0

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
getMoveList :: Board -> Player -> Int -> (Int, Int) -> [(Int, Int)]
getMoveList board player index (fromX, fromY)
    -- * player is currently playing a pawn
    | (index > 1) = 
        [(fromX, fromY + forward), (fromX - 1, fromY + forward), (fromX + 1, fromY + forward)]
    -- * otherwise, player is currently playing a knight
    | otherwise = 
        [(fromX + 1, fromY + 2), (fromX + 1, fromY - 2), (fromX - 1, fromY - 2), (fromX -1, fromY +2), (fromX + 2, fromY + 1), (fromX + 2, fromY - 1), (fromX -2, fromY - 1), (fromX - 2, fromY + 1)]
    where startPiece = getFromBoard board (fromX, fromY)
          forward = case player of
                        Black -> -1
                        White -> 1
