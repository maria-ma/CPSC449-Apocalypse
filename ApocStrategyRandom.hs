module ApocStrategyRandom  where

import ApocTools

random :: Chooser
random gamestate Normal player        = return (Just [(0,0),(1,1)])
random gamestate PawnPlacement player = return (Just [(2,2)])
