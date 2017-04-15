module ApocStrategyRandom  where

import Data.Maybe (fromJust, isNothing)
import Data.Char
import System.Environment
import ApocTools
import AiFunctions
import System.Random
import MoveValidations

{- | 
     randomStr: implementation of the random strategy (strategy #1)
     params: the gamestate, play type, and player
-}

randomStr :: Chooser
-- * Normal playtype returns the source and destination coordinates of a player's move
randomStr gamestate Normal player        = do -- return (Just [(0,0),(2,1)]) --do
    let pieces = getPieces board (player) Normal
    --putStrLn "tryna get random play piece"
    --randNum <- genIndex pieces
    playPiece <- chooseRandom pieces
    --putStrLn ("length of list: " ++ (show $ length pieces) ++ " index num: " ++  (show randNum) ++ " chosen: " ++ (show playPiece))
    let moves = getMoveList board player playPiece
    let possMoves = canMove board player playPiece moves
    --putStrLn "tryna get random move from play piece"
    --randNum2 <- genIndex moves
    if (length possMoves == 0) then return Nothing
    else 
        do
            move <- chooseRandom possMoves
            return (Just [playPiece,move])
                        -- * otherwise, generate a new move
            --            else randomStr gamestate Normal player
    where board = (theBoard gamestate)
-- * PawnPlacement playtype returns a cell on the board indicating the nearest empty space a pawn can go to
randomStr gamestate PawnPlacement player = do --return (Just [(2,2)])
    randX <- chooseRandom4
    randY <- chooseRandom4 
    let playPiece = (randX, randY)
    putStrLn $ "play piece: " ++ show playPiece
    if ((checkEmptySpace (theBoard gamestate) playPiece) == True) then return (Just [playPiece])
    else randomStr gamestate PawnPlacement player
--    let move = placePawn (theBoard gamestate) player 0 0

canMove :: Board -> Player -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
canMove board player start [] = []
canMove board player start (x:xs)
    | (checkCoordRange (fst x) (snd x) == True) = do 
        if (checkMoveLegal board player start x == True) then x : canMove board player start xs
        else canMove board player start xs
    | otherwise = canMove board player start xs


-- | checkCoordRange: range checks the destination coordinates
--   if an x or y coordinate is either >4 or <0, then return false
checkCoordRange :: Int -> Int -> Bool
checkCoordRange toX toY
    | (toX > 4) || (toY > 4) = False
    | (toX < 0) || (toY < 0) = False
    | otherwise = True

-- | chooseRandom: chooses a random element from a given list
chooseRandom :: [a] -> IO a
chooseRandom list = do
    index <- randomRIO (0, (length list) - 1)
    return (list !! index)
chooseRandom4 :: IO Int
chooseRandom4 = getStdRandom (randomR (0,4))

--genIndex :: [a] -> IO Int
--genIndex list = getStdRandom (randomR (0, (length list) - 1))

--chooseMove :: Board -> Player -> IO (Maybe [(Int, Int)])
--chooseMove board player = do -- checks if a move can be made
-- if not, make a pass