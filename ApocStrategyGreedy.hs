{- |
Module      : ApocStrategyGreedy
Description : The greedy strategy strategy (that is not implemented)
Copyright   : Copyright 2017, Gil Abinal and Maria Mamaclay, University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Portability : ghc 7.10.2 - 7.10.3
Limitations : Not implemented!!!!!!!!!! Only returns a set coordinate
This module is used for CPSC 449 for the Apocalypse assignment.
-}

module ApocStrategyGreedy where

import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import ApocTools
import AiFunctions
import System.Random
import MoveValidations
import ApocStrategyRandom


{- | 
     greedy: implementation of the greedy strategy (not implemented)
     params: the gamestate, play type, and player
-}

greedy :: Chooser
greedy gamestate Normal player = return (Just [(0,0),(2,1)])
greedy gamestate PawnPlacement player = return (Just [(2,2)])