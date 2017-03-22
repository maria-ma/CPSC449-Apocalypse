module ApocStrategyRandom  where

import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import ApocTools
import AiFunctions
import System.Random

-- convert int to io (randio was io)

random :: Chooser
random gamestate Normal player        = do --return (Just [(0,0),(2,1)]) --do
    let pieces = getPieces (theBoard gamestate)(player)
    playPiece <- chooseRandom pieces
    let moves = getMoveList (theBoard gamestate) player playPiece
    (toX, toY) <- chooseRandom moves
    return (Just [playPiece,(toX, toY)])
random gamestate PawnPlacement player = return (Just [(2,2)])

-- to do: get list of available moves
-- choose randomly
-- or we can choose a random piece from the player
-- randomly generate legal coordinates to go to
-- then if the move if legal go make the move
-- otherwise generate another move

chooseRandom :: [a] -> IO a
chooseRandom list = do
    index <- randomRIO (0, (length list) - 1)
    return (list !! index)

--chooseMove :: Board -> Player -> IO (Maybe [(Int, Int)])
--chooseMove board player = do -- checks if a move can be made
-- if not, make a pass