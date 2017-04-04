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

Limitations : This implementation only implements the human strategy
              Additionally, this implementation only prints out the initial board of the gameplay, and will not loop through an entire game
              When in interactive mode, the program will print out the initial move as well as the board when a black player does its first move
              (the move for the white player will not be prompted, and will result as a PASS)
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
import AiFunctions
import MoveValidations

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
        blackStr <- askStrategies "black"
        whiteStr <-askStrategies "white"
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
        -- pass vakues onto game loop here 
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
checkStrategyValid "random" = return randomStr -- and return it to the main function
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

---Game Loop Functions-------------------------------------------------------------

-- have to do: check the end game conditions (go to end game function)
-- then check on the collisions and pawn placements
-- check for goofs
-- check for collisions
-- and everything else
-- print and go back to loop game 

-- LOSE CONDITIONS
-- One of the players loses all his/her pawns.  The other player is the winner. 
-- One of the players accumulates two penalty points.  The other player is the winner.
-- Both players pass on the same round. The one with the most pawns wins.

gameLoop :: GameState -> Chooser -> Chooser -> PlayType -> Bool -> IO()
gameLoop currentBoard black white playType end = do

    -- check if players lost all their pawns
    let findBlackPawns = findPawns (theBoard currentBoard) Black
    let findWhitePawns = findPawns (theBoard currentBoard) White
    -- check if players accumulated >2 penalty points
    let blackMaxPenalty = currentBoard blackPen >= 2
    let whiteMaxPenalty = currentBoard whitePen >= 2

    if (end == False) then

        -- check a player met a lose game condition
        if (findBlackPawns == False || findWhitePawns == False || blackMaxPenalty == True || whiteMaxPenalty == True)
          gameLoop currentBoard black white playType True

        else
            do
                -- retrieve the black and white player's moves
                blackMove <- black currentBoard playType Black
                whiteMove <- white currentBoard playType White
                -- check if both players passed (human players only)
                if (blackMove == Nothing && whiteMove == Nothing)
                    do
                        show currentBoard
                        gameLoop currentBoard black white playType True
                --else 
                -- get player's source coordinates
                --let blackOrigin = blackMove !! 0
                --let whiteOrigin = whiteMove !! 0
                --let blackDest = blackMove !! 1
                --let whiteDest = whiteMove !! 1

                -- check for validity 
                --let blackValid = checkMoveLegal currentBoard Black blackOrigin blackDest
                --let whiteValid = checkMoveLegal currentBoard White whiteOrigin whiteDest

                --if (blackValid == False || whiteValid == False)
                --   if (blackValid == False)
                --       do 
                -- get msg to Goofed, show coords, increase penalty point for player
                -- replace old piece with space
                -- update old space with new coordinate

    -- | end == True
    --   in this case, find the winners and print out the game over message
    else 
        do   

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
