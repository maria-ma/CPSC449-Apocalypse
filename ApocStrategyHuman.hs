{- |
Module      : ApocStrategyHuman
Description : Template for a game-playing strategy definition.
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

This is merely a skeleton to get you started on creating a strategy for playing the
Apocalypse game.  It has VERY little functionality.
-}

module ApocStrategyHuman (
   human
   ) where

import ApocTools
import Data.Char
import Data.List
import Data.Maybe

{- | This is just a placeholder for the human strategy: it always chooses to play
     (0,0) to (2,1).
-}
human    :: Chooser
human state Normal player = makeNormalMove state player
human state PawnPlacement player = makePawnPlaceMove state player

-- | lets a human player make a pawn placement move
--   params: takes the current gameState, which player is acting
--   returns: a list of int that contains the destination coordinate of the pawn placement
makePawnPlaceMove :: GameState -> Player -> IO (Maybe[(Int, Int)])
makePawnPlaceMove state player = do 
    -- * gets input from user
    putStrLn("Enter the coordinates to place the pawn for player " ++ (show player) ++ " in the form 'destX destY'\n[0 >= n >= 4] " ++ promptEnd ++ ":")
    choice <- getLine
    let inputConverted = convertInput choice                     -- ^ converts input from a string to a list of ints
    let output = listSplit inputConverted                        -- ^ splits list into distinct sets of coordinates
    if (null inputConverted)
        then do 
            return output
        else if (checkRange inputConverted == True)              -- ^ make sure the input is within range of the board
            then do                                              -- ^ if not, re-run getting user input
                putStrLn(choice ++ " integers out of range")
                makeNormalMove state player
            else if (length inputConverted /= 2)                 -- ^ will also check if the right number of inputs is inputted
                then do
                putStrLn(choice ++ " " ++ intsToString inputConverted ++ " integers found, 2 required.")
                makeNormalMove state player
                else return output                               -- ^ else, continue with the pawn placement
    where promptEnd = case player of
            White -> "W1"
            Black -> "B1"

-- | lets a human player make a move with inputs
--   params: takes the current gameState, which player is acting
--   returns: a list of ints for moving pieces
makeNormalMove :: GameState -> Player -> IO (Maybe[(Int,Int)])
makeNormalMove state player = do
    -- * gets input from user
    putStrLn("Enter the move coordinates for player " ++ (show player) ++ " in the form 'srcX srcY destX destY'\n[0 >= n >= 4, or just enter return for a 'pass'] " ++ promptEnd ++ ":")
    choice <- getLine
    let inputConverted = convertInput choice                 -- ^ converts input from a string to a list of ints
    let output = listSplit inputConverted                    -- ^ splits list into distinct sets of coordinates
    if (null inputConverted)                                 -- ^ if the input is null, return the output (Nothing)
        then do     
            return output                                         
        else if (checkRange inputConverted == True)          -- ^ make sure the input is within range of the board
            then do
                putStrLn(choice ++ " integers out of range")
                makeNormalMove state player                  -- ^ if not, re-run getting user input
            else if (length inputConverted /= 4)             -- ^ will also check if the right number of inputs is inputted
                then do
                putStrLn(choice ++ " " ++ intsToString inputConverted ++ " integers found, 4 required.")
                makeNormalMove state player
                else return output                           -- ^ else, continue with the move
    where promptEnd = case player of
            White -> "W2"
            Black -> "B2"

-- | converts the length of input into its associated string value (from 1-4)
intsToString :: [Int] -> String
intsToString x 
    | ints == 1 = "One"
    | ints == 2 = "Two"
    | ints == 3 = "Three"
    | ints == 4 = "Four"
    | ints > 4  = "Greater than four"
    | ints < 1  = "Less than one"
    where ints = length x

-- | simply converts a string input into a list of integers for input into other functions
--   this also takes free-form comments into account
--   params: a string user input indicating the coordinates they would like to move their piece to
--   returns: a list of integers based on the input
convertInput :: String -> [Int]
convertInput "" = []
convertInput xs = do
    if (isInfixOf "--" xs == True)                            -- ^ first checks if a free form comment is in the input (signalled by "--")
        then do
            map read $ (take findComment $ words xs) :: [Int] -- ^ converts the sting input into integer up until it encounters the comment signal
        else map read $ words xs :: [Int]                     -- ^ if there's no free form comment, proceed as normal
    where findComment = fromJust $ elemIndex "--" (words xs)

-- | splits a list of integers into two distinct coordinates
--   params: list of integers (should be length of 2 or 4)
--   returns: a list of lists of integers, split in half, if the list is 4 long
listSplit :: [Int] -> Maybe [(Int, Int)]
listSplit [] = Nothing
listSplit [a, b] = Just [(a, b)]
listSplit [a,b,c,d] = Just [(a,b),(c,d)]

-- | checks to make sure a coordinate ot move to is in range of the game board (between 0 and 4)
--   params: list of integers
--   returns: a boolean, true if the piece falls outside the board, false if its okay
checkRange :: [Int] -> Bool
checkRange moveCoord 
    | (head (sort moveCoord)) < 0 = True
    | (last (sort moveCoord)) > 4 = True
    | otherwise = False
