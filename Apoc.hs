{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment. 
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

-}

module Main (
      -- * Main
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import System.IO.Unsafe
import System.Exit
import ApocTools
import ApocStrategyHuman
import ApocStrategyRandom

---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args
    -- * In the case that there are no input arguments, prompt user for strategy names 
    | lengthArgs == 0 = do
        -- * Prints description and prompts user to input the strategies for the black and white players
        putStrLn "Possible strategies:"
        putStr printStrategies
        askStrategies "black"
        askStrategies "white"
        -- * Prints initial board
        print initBoard
        --putStrLn $ "\nThe initial board with back human (the placeholder for human) strategy having played one move\n"
        --           ++ "(clearly illegal as we must play in rounds!):"
        move <- human (initBoard) Normal Black
        putStrLn (show $ GameState (if move==Nothing
                                    then Passed
                                    else Played (head (fromJust move), head (tail (fromJust move))))
                                   (blackPen initBoard)
                                   (Passed)
                                   (whitePen initBoard)
                                   (replace2 (replace2 (theBoard initBoard)
                                                       ((fromJust move) !! 1)
                                                       (getFromBoard (theBoard initBoard) ((fromJust move) !! 0)))
                                             ((fromJust move) !! 0)
                                             E))
    -- | Takes two inputted strategy names from command line and checks if they're valid
    --   it will then print out the initial state of the board
    | lengthArgs == 2 = do
        blackStr <- checkStrategyValid getBlackStr
        whiteStr <- checkStrategyValid getWhiteStr
        print initBoard
    -- * Otherwise, prints out the list of possible strategies
    | otherwise = putStrLn ("\nInvalid number of arguments for strategies. Possible strategies are:" ++ printStrategies)
    where lengthArgs  = length args
          getBlackStr = map toLower (head args)
          getWhiteStr = map toLower (last args)

---User Prompt Functions------------------------------------------------------------

-- | checks if the user's inputted strategy is valid
--   if the inputted strategy is not "human", "random", or "greedy", the program will print the valid strategies
--   and exit
checkStrategyValid :: String -> IO(Chooser)
checkStrategyValid "human" = return human  -- TODO: implement the game strategies
checkStrategyValid "random" = return random -- and return it to the main function
checkStrategyValid "greedy" = return greedy -- (will be type Chooser)
checkStrategyValid x = die("\n" ++ x ++ " is an invalid strategy name. Valid list of strategies:" ++ printStrategies)

-- | string of the list of available, playable strategies
printStrategies :: String
printStrategies = let strategies = ["human","random","greedy"]
              in (foldr (++) "" ((map (\x -> "  " ++ x ++ "\n") strategies)))

-- | prints a welcome message and the list of playable strategies in interactive mode
--printDesc :: IO()
--printDesc = putStrLn ("\nWelcome to the Apocalypse Simulator! Please choose a strategy type for the black and white players:")

-- | (in interactive mode) prompts user to input a strategy
askStrategies :: String -> IO(Chooser)
askStrategies player = do 
        putStrLn ("Please enter a strategy for the " ++ player ++ " player: ")
        strategyIn <- getLine
        strategy <- checkStrategyValid strategyIn
        return strategy

---Movement Validation functions----------------------------------------------------

-- | checkMoveLegal: checks if a normal move is legal
--   params: the board, player, start coordinates, end coordinates
--   returns: boolean indicating if the indicated move is legal
checkMoveLegal :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkMoveLegal board player start to 
    | (cell2Char startCell) == '_' = False
    | ((playerOf startPiece) == player) && (startCell == WP || startCell == BP) = checkPawnLegal board player start to
    | ((playerOf startPiece) == player) && (startCell == WK || startCell == BK) = checkKnightLegal board player start to
    | otherwise = False
    where startCell  = getFromBoard board start
          startPiece = pieceOf startCell

-- | checkEmptySpace: checks if the destination coordinate is empty
--   (this function will also work for checking if a PawnPlacement move is legal
--   params: the board, destination coordinate
--   returns: boolean if the move was legal
checkEmptySpace :: Board -> (Int, Int) -> Bool
checkEmptySpace board x
    | getFromBoard board x == E = True
    | otherwise = False

-- | checkOpponent: checks if the destination coordinate contains an opponent
--   params: board, current player, destination coordinate
--   returns: boolean if the destination coordinate contains an opponent piece
checkOpponent :: Board -> Player -> (Int, Int) -> Bool
checkOpponent board player to =  (player /= getPlayer)
                                 where getPlayer = playerOf $ pieceOf $ getFromBoard board to

-- | checkPawnLegal: checks if the player's intended pawn movement is valid
--   params: the board, current player, and the start and destination coordinates
--   returns: boolean on pawn movement validity
checkPawnLegal :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkPawnLegal board player (fromX, fromY) to
    | (to == (fromX, fromY + forward)) && (checkEmptySpace board to)         = True -- ^ move to an empty space    ( 0,+1)
    | to  == (fromX - 1, fromY + forward) && (checkOpponent board player to) = True -- ^ eat a piece to the left   (-1,+1)   
    | to  == (fromX + 1, fromY + forward) && (checkOpponent board player to) = True -- ^ each a piece to the right (+1,+1)
    | otherwise = False
    where forward = case player of
                      Black -> -1
                      White -> 1

-- | checkKnightLegal: checks if the player's intended knight movement is valid
--   params: the board, curren player, start and end desination cell coordinates
--   returns: boolean on knight movement validity
checkKnightLegal :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkKnightLegal board player (fromX, fromY) to
   | (to == (fromX + 1, fromY + 2)) && (checkTo) = True -- ^ (+1,+2)
   | (to == (fromX + 1, fromY - 2)) && (checkTo) = True -- ^ (+1,-2)
   | (to == (fromX - 1, fromY - 2)) && (checkTo) = True -- ^ (-1,-2)
   | (to == (fromX - 1, fromY + 2)) && (checkTo) = True -- ^ (-1,+2)
   | (to == (fromX + 2, fromY + 1)) && (checkTo) = True -- ^ (+2,+1)
   | (to == (fromX + 2, fromY - 1)) && (checkTo) = True -- ^ (+2,-1)
   | (to == (fromX - 2, fromY - 1)) && (checkTo) = True -- ^ (-2,-1)
   | (to == (fromX - 2, fromY + 1)) && (checkTo) = True -- ^ (-2,+1)
   | otherwise = False
   where checkTo = ((checkEmptySpace board to) || (checkOpponent board player to))

---Player Strategy functions-------------------------------------------------------

greedy :: Chooser
greedy a Normal b        = return (Just [(0,0),(1,1)]) -- ^ TODO: finish the functions for greedy strategy
greedy a PawnPlacement b = return (Just [(2,2)])

---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)
