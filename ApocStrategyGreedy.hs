module ApocStrategyGreedy where

import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import ApocTools
import AiFunctions
import System.Random
import MoveValidations
import ApocStrategyRandom

greedy :: Chooser
greedy gamestate Normal player = return (Just [(0,0),(2,1)])
greedy gamestate PawnPlacement player = return (Just [(2,2)])