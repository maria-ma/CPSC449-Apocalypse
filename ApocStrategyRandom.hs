{- |
Module      : Random Strategy Module
Description : This module implements the random strategy for the AI
Copyright   : Copyright 2017, Gil Abinal and Maria Mamaclay, University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3
This module is used for CPSC 449 for the Apocalypse assignment.

-}

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
randomStr gamestate Normal player        = do 
    -- | gets all the pieces that the player can play from the board
    let pieces = getPieces board (player) Normal
    -- | chooses a random element from the list of pieces the player can play
    playPiece <- chooseRandom pieces
    
    -- | gets the movelist and the possible moves of the piece that is being played
    let moves = getMoveList board player playPiece
    let possMoves = canMove board player playPiece moves
    -- | if the length of the possible moves is 0, return Nothing (pass)
    -- | else return a list of Int tuples
    if (length possMoves == 0) then return Nothing
    else 
        do
            move <- chooseRandom possMoves
            return (Just [playPiece,move])
    where board = (theBoard gamestate)
    
-- * PawnPlacement playtype returns a cell on the board indicating the nearest empty space a pawn can go to
randomStr gamestate PawnPlacement player = do 
    -- | generates two random integers to be used as x and y coordinates
    randX <- chooseRandom4
    randY <- chooseRandom4   
    let playPiece = (randX, randY)
    -- | checks if the generated coordinate is an empty space on the board then return it
    -- | else call the function again
    if ((checkEmptySpace (theBoard gamestate) playPiece) == True) then return (Just [playPiece])
    else randomStr gamestate PawnPlacement player


-- | canMove: returns a list of possible moves the player can play
canMove :: Board -> Player -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
canMove board player start [] = []
canMove board player start (x:xs)
    -- | check if the values are not out of range
    -- | if true, check if the values are legal moves then add it to the list
    -- | else move to the next value
    | (checkCoordRange (fst x) (snd x) == True) = do 
        if (checkMoveLegal board player start x == True) then x : canMove board player start xs
        else canMove board player start xs
    | otherwise = canMove board player start xs


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

-- | generate a random number from 0 to 4.
-- | the numbers generated will be used as coordiates
chooseRandom4 :: IO Int
chooseRandom4 = getStdRandom (randomR (0,4))

--genIndex :: [a] -> IO Int
--genIndex list = getStdRandom (randomR (0, (length list) - 1))

--chooseMove :: Board -> Player -> IO (Maybe [(Int, Int)])
--chooseMove board player = do -- checks if a move can be made
-- if not, make a pass
