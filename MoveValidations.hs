{- |
Module      : MoveValidations
Description : The functions needed validate moves
Copyright   : Copyright 2017, Gil Abinal and Maria Mamaclay, University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.
-}

module MoveValidations where

import ApocTools
import Data.Char
import Data.List
import Data.Maybe
import System.Environment

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
--(player /= getPlayer)
--where getPlayer = playerOf $ pieceOf $ getFromBoard board to
checkOpponent :: Board -> (Int, Int) -> (Int, Int) -> Bool
checkOpponent board start dest
    | (startCell == WP || startCell == WK) && (destCell == BP || destCell == BK) = True
    | (startCell == BP || startCell == BK) && (destCell == WP || destCell == WK) = True
    | otherwise = False
    where startCell = getFromBoard board start
          destCell = getFromBoard board dest

-- | checkPawnLegal: checks if the player's intended pawn movement is valid
--   params: the board, current player, and the start and destination coordinates
--   returns: boolean on pawn movement validity
checkPawnLegal :: Board -> Player -> (Int, Int) -> (Int, Int) -> Bool
checkPawnLegal board player (fromX, fromY) to
    | (to == (fromX, fromY + forward)) && (checkEmptySpace board to)         = True -- ^ move to an empty space    ( 0,+1)
    | (to == (fromX - 1, fromY + forward)) && (checkOpponent board (fromX, fromY) to) = True -- ^ eat a piece to the left   (-1,+1)   
    | (to == (fromX + 1, fromY + forward)) && (checkOpponent board (fromX, fromY) to) = True -- ^ each a piece to the right (+1,+1)
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
   where checkTo = ((checkEmptySpace board to) || (checkOpponent board (fromX, fromY) to))