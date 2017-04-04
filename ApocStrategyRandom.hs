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
    playPiece <- chooseRandom pieces
    let moves = getMoveList board player playPiece
    (toX, toY) <- chooseRandom moves
    -- TODO check if pawn or knight placement is valid (either opponent is there for pawn diag or is empty)
    -- if it fails, run random gamestate again? (can do this in the apoc loop)
    if ((getFromBoard board playPiece == WP) || (getFromBoard board playPiece == BP) && (checkPawnLegal board player playPiece (toX, toY) == True))
        then do return (Just [playPiece,(toX, toY)])
        else if ((getFromBoard board playPiece == WK) || (getFromBoard board playPiece == BK) && (checkKnightLegal board player playPiece (toX, toY) == True))
            then do return (Just [playPiece,(toX, toY)])
            else randomStr gamestate Normal player
    where board = (theBoard gamestate)
-- * PawnPlacement playtype returns a cell on the board indicating the nearest empty space a pawn can go to
randomStr gamestate PawnPlacement player = do --return (Just [(2,2)])
    let pieces = getPieces (theBoard gamestate) player PawnPlacement
    playPiece <- chooseRandom pieces
    let move = placePawn (theBoard gamestate) player 0 0
    return (Just [playPiece, move])

-- to do: get list of available moves
-- choose randomly
-- or we can choose a random piece from the player
-- randomly generate legal coordinates to go to
-- then if the move if legal go make the move
-- otherwise generate another move

-- | chooseRandom: chooses a random element from a given list
chooseRandom :: [a] -> IO a
chooseRandom list = do
    index <- randomRIO (0, (length list) - 1)
    return (list !! index)

--chooseMove :: Board -> Player -> IO (Maybe [(Int, Int)])
--chooseMove board player = do -- checks if a move can be made
-- if not, make a pass