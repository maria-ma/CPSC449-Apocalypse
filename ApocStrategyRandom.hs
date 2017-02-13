module ApocStrategyRandom  where

import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import ApocTools
import System.Random

random :: Chooser
random gamestate Normal player        = return (Just [(0,0),(1,1)])
random gamestate PawnPlacement player = return (Just [(2,2)])

-- to do: get list of available moves
-- choose randomly
-- or we can choose a random piece from the player
-- randomly generate legal coordinates to go to
-- then if the move if legal go make the move
-- otherwise generate another move


