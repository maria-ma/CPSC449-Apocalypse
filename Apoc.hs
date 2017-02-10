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
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman
import System.Exit


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:
     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args
    | lengthArgs == 0 = do
        printDesc
        askStrategies "black"
        askStrategies "white"
        putStrLn "\nThe initial board:"
        print initBoard

        putStrLn $ "\nThe initial board with back human (the placeholder for human) strategy having played one move\n"
                   ++ "(clearly illegal as we must play in rounds!):"
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
    | lengthArgs == 2 = do
        putStrLn "\nin chosen strategy mode"
        putStrLn "\ncheck the inputted strategies if valid\nand print initial board"
        blackStr <- checkStrategyValid $ head args
        whiteStr <- checkStrategyValid $ last args
        print initBoard
    | otherwise = putStrLn ("\nInvalid number of arguments for strategies. Possible strategies are:" ++ printStrategies)
    where lengthArgs = length args

--Additional Functions-------------------------------------------------------------

checkStrategyValid :: String -> IO(Chooser)
checkStrategyValid "human" = return human  -- TODO: implement the game strategies
checkStrategyValid "random" = return random -- and return it to the main function
checkStrategyValid "greedy" = return greedy -- (will be type Chooser)
checkStrategyValid x = die("\n" ++ x ++ " is an invalid strategy name. Valid list of strategies:" ++ printStrategies)

printStrategies :: String
printStrategies = let strategies = ["human","random","greedy"]
              in (foldr (++) "" ((map (\x -> "\n  " ++ x) strategies)))

printDesc :: IO()
printDesc = putStrLn ("\nWelcome to the Apocalypse Simulator! Please choose a strategy type for the black and white players:" ++ printStrategies)

askStrategies :: String -> IO(Chooser)
askStrategies player = do 
        putStrLn ("\nPlease enter a strategy for the " ++ player ++ " player: ")
        strategyIn <- getLine
        strategy <- checkStrategyValid strategyIn
        return strategy

---Player Strategy functions-------------------------------------------------------

greedy :: Chooser
greedy a Normal b        = return (Just [(0,0),(1,1)]) -- ^ TODO: finish the functions for greedy strategy
greedy a PawnPlacement b = return (Just [(2,2)])

random :: Chooser
random a Normal b        = return (Just [(0,0),(1,1)]) -- ^ TODO: finish the functions for random strategy
random a PawnPlacement b = return (Just [(2,2)])

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
