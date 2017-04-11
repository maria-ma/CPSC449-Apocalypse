module ApocStrategyRandom  where

import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import ApocTools
import AiFunctions
import System.Random
import MoveValidations

{- | 
     randomStr: implementation of the random strategy (strategy #1)
     params: the gamestate, play type, and player
-}

randomStr :: Chooser
-- * Normal playtype returns the source and destination coordinates of a player's move
randomStr gamestate Normal player        = do -- return (Just [(0,0),(2,1)]) --do
    let pieces = getPieces board (player) Normal
    --putStrLn "tryna get random play piece"
    --randNum <- genIndex pieces
    playPiece <- chooseRandom pieces
    --putStrLn ("length of list: " ++ (show $ length pieces) ++ " index num: " ++  (show randNum) ++ " chosen: " ++ (show playPiece))
    let moves = getMoveList board player playPiece
    --putStrLn "tryna get random move from play piece"
    --randNum2 <- genIndex moves
    (toX, toY) <- chooseRandom moves 
    --putStrLn ("length of list: " ++ (show $ length moves) ++ " index num: " ++  (show randNum2) ++ " chosen: " ++ (show (toX, toY)))

    -- * checking if the given destination coordinates are valid
    --   first check the range
    if (checkCoordRange toX toY == False) then randomStr gamestate Normal player
        -- checking if the intended pawn move is legal
        else if (((getFromBoard board playPiece == WP) || (getFromBoard board playPiece == BP)) && (checkPawnLegal board player playPiece (toX, toY) == True))
            then return (Just [playPiece,(toX, toY)])
            -- * checking if the intended knight move is legal
            else if (((getFromBoard board playPiece == WK) || (getFromBoard board playPiece == BK)) && (checkKnightLegal board player playPiece (toX, toY) == True))
                then return (Just [playPiece,(toX, toY)])
                -- * otherwise, generate a new move
                else randomStr gamestate Normal player
    where board = (theBoard gamestate)
-- * PawnPlacement playtype returns a cell on the board indicating the nearest empty space a pawn can go to
randomStr gamestate PawnPlacement player = do --return (Just [(2,2)])
    let pieces = getPieces (theBoard gamestate) player PawnPlacement
    playPiece <- chooseRandom pieces
    let move = placePawn (theBoard gamestate) player 0 0
    return (Just [playPiece, move])

-- | checkCoordRange: range checks the destination coordinates
--   if an x or y coordinate is either >4 or <0, then return false
checkCoordRange :: Int -> Int -> Bool
checkCoordRange toX toY
    | (toX > 4) || (toY > 4) = False
    | (toX < 0) || (toY < 0) = False
    | otherwise = True

-- | chooseRandom: chooses a random element from a given list
chooseRandom :: [a] -> IO a
chooseRandom list = do
    index <- randomRIO (0, (length list) - 1)
    return (list !! index)

--genIndex :: [a] -> IO Int
--genIndex list = getStdRandom (randomR (0, (length list) - 1))

--chooseMove :: Board -> Player -> IO (Maybe [(Int, Int)])
--chooseMove board player = do -- checks if a move can be made
-- if not, make a pass