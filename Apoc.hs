{- |
Module      : Main
Description : The main game loop and interaction functionalities 
Copyright   : Copyright 2017, Gil Abinal and Maria Mamaclay, University of Calgary.
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
Limitations : This implementation only implements the human and random strategy
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
import ApocStrategyGreedy
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
        -- * Asking for black strategy, and copies both the name of the strategy and the chooser type
        putStrLn ("Please enter a strategy for the black player: ")
        blackSName <- getLine
        blackStr <- checkStrategyValid blackSName
        -- * Asking for white strategy
        putStrLn ("Please enter a strategy for the white player: ")
        whiteSName <- getLine
        whiteStr <- checkStrategyValid whiteSName
        -- * passing both inputs for the black and white strategy into the main game loop
        gameLoop initBoard blackStr blackSName whiteStr whiteSName Normal False
    -- | Takes two inputted strategy names from command line and checks if they're valid
    --   and passes it to the main game loop
    | lengthArgs == 2 = do
        [blackSName, whiteSName] <- getArgs
        blackStr <- checkStrategyValid blackSName
        whiteStr <- checkStrategyValid whiteSName
        gameLoop initBoard blackStr blackSName whiteStr whiteSName Normal False
    -- * Otherwise, prints out the list of possible strategies
    | otherwise = putStrLn ("\nInvalid number of arguments for strategies. Possible strategies are:" ++ printStrategies)
    where lengthArgs  = length args               -- ^ the number of arguments from the command line
          getBlackStr = map toLower (head args)   -- ^ the inputted black strategy (not case sensitive)
          getWhiteStr = map toLower (last args)   -- ^ the inputted white strategy

---User Prompt Functions------------------------------------------------------------

-- | checkStrategyValid: checks if the user's inputted strategy is valid
--   if the inputted strategy is not "human", "random", or "greedy", the program will print the valid strategies available and exit
checkStrategyValid :: String -> IO(Chooser)
checkStrategyValid "human" =  return human  
checkStrategyValid "random" = return randomStr
checkStrategyValid "greedy" = return greedy
checkStrategyValid x = die("\n" ++ x ++ " is an invalid strategy name. Valid list of strategies:" ++ printStrategies)

-- | string of the list of available, playable strategies
printStrategies :: String
printStrategies = let strategies = ["human","random","greedy"]
              in (foldr (++) "" ((map (\x -> "  " ++ x ++ "\n") strategies)))

-- | (in interactive mode) prompts user to input a strategy
askStrategies :: String -> IO(Chooser)
askStrategies player = do 
        putStrLn ("Please enter a strategy for the " ++ player ++ " player: ")
        strategyIn <- getLine
        strategy <- checkStrategyValid strategyIn
        return strategy

---Game Loop Functions-------------------------------------------------------------

{- | The main game loop function that interacts with the players and their inputs
     Does the following:
        1. prints the current board of the last round
        2. checks if this board meets any of the lose conditions
        3. if the board does not meet the loop conditions, it will:
            a. gather the players' moves for the next round
            b. update the game state based on these moves
            c. if a PAWNPLACEMENT is needed, check through all the possible cases before updating the board
            d. recurse back with the updated game state (ie. with the player's next inputted moves)
        4. otherwise, the board will print out the associated game over message
-}

gameLoop :: GameState -> Chooser -> String -> Chooser -> String -> PlayType -> Bool -> IO()
gameLoop currentBoard black blackStr white whiteStr playType end = do
    putStrLn $ show currentBoard
    -- the main game loop
    if (end == False) then
        do
            --  | checks a player met a lose game condition on the current board:
            --    (1) One of the players loses all his/her pawns.  The other player is the winner. 
            --    (2) One of the players accumulates two penalty points.  The other player is the winner.
            if (notEnd currentBoard) then
                do
                    -- * retrieve the black and white player's moves                 
                    blackMove <- black currentBoard playType Black
                    whiteMove <- white currentBoard playType White
                    -- * update the game state based on the inputted moves moves 
                    let updatedGS = updateGS currentBoard playType blackMove whiteMove
                        
                    -- | checks the final lose game condition on the current board:
                    --   (3) Both players pass on the same round. The one with the most pawns wins.
                    --   should the board meet this condition, it will loop back and indicate that the end game condition has been met
                    if (blackMove == Nothing && whiteMove == Nothing) then
                        do
                            let updatedGS = GameState Passed (blackPen currentBoard) Passed (whitePen currentBoard) (theBoard currentBoard)
                            gameLoop updatedGS black blackStr white whiteStr playType True
                    else 
                        do                              
                            -- | checks if there are any pawns present in a row that needs PAWN PLACEMENT (ie. a black pawn is in the white player's starting row, etc)
                            --   if there are not any pawns in such a row, the game will be considered a NORMAL play type
                            if ((numInRow BP $ (theBoard updatedGS)!!0) == 0 ) && ((numInRow WP $ (theBoard updatedGS)!!4) == 0) then
                                gameLoop updatedGS black blackStr white whiteStr Normal False -- ^ since this is a normal play type, go back and recursive as we do not need to update for pawn placement
                            -- * this is the case where we need a PAWN PLACEMENT or promotion, and we will have to consider the following factors:
                            else 
                                do
                                    -- * first check if the updated game state has NOT met an end game condition
                                    if (notEnd updatedGS) then
                                        do
                                            -- * if so, print out the board needed for pawn placement
                                            putStrLn $ show updatedGS
                                            -- | in these following cases, the game checks if BOTH players need a pawn promotion or a pawn placement
                                            --   should the updated game state meet this case, the game will ask and update the pawn based on four factors:
                                            --       (1)   if both pawns are able to promote to the knight
                                            --       (2-3) if a black pawn needs to promotes, but a white pawn needs placement (and the other way around
                                            --       (4)   both pawns need a pawn placement
                                            --   these cases will each do the following: 
                                            --       (a) update a temporary game state for the black move
                                            --       (b) update that temporary state for the white move
                                            --       (c) compile both of these moves into a final updated game state, and pass that into the loop
                                            if (((numInRow BP $ (theBoard updatedGS)!!0) > 0) && ((numInRow WP $ (theBoard updatedGS)!!4) > 0)) then
                                                do
                                                    -- * case 1: two pawn promotions needed
                                                    if ((numOnBoard WK (theBoard updatedGS) == 2) && (numOnBoard BK (theBoard updatedGS) == 2)) then
                                                        do
                                                            let updatedGS' = promotePawn updatedGS (theBoard updatedGS) (findPawnPromote (head (theBoard updatedGS)) True False 0)
                                                            let updatedGS'' = promotePawn updatedGS (theBoard updatedGS) (findPawnPromote (last (theBoard updatedGS)) False True 0) 
                                                            let finalUpdate = GameState (blackPlay updatedGS') (blackPen updatedGS') (whitePlay updatedGS'') (whitePen updatedGS'') (theBoard updatedGS'')                                                    
                                                            gameLoop finalUpdate black blackStr white whiteStr Normal False
                                                    -- * case 2: black promotion and white pawn placement
                                                    else if ((numOnBoard WK (theBoard updatedGS) < 2) && (numOnBoard BK (theBoard updatedGS) == 2)) then
                                                        do
                                                            let updatedGS' = promotePawn updatedGS (theBoard updatedGS) (findPawnPromote (head (theBoard updatedGS)) True False 0)
                                                            whiteMove <- white updatedGS' PawnPlacement White
                                                            let updatedGS'' = do
                                                                case whiteMove of
                                                                    Nothing -> updatePP updatedGS' (getPawnPlace (blackPlay updatedGS')) Nothing False True
                                                                    maybe -> updatePP updatedGS' (getPawnPlace (blackPlay updatedGS')) (Just [(findPawnPromote (last (theBoard updatedGS')) False True 0), (head $ fromJust whiteMove)]) False True
                                                            let finalUpdate = GameState (blackPlay updatedGS') (blackPen updatedGS') (whitePlay updatedGS'') (whitePen updatedGS'') (theBoard updatedGS'')  
                                                            gameLoop finalUpdate black blackStr white whiteStr Normal False
                                                    -- * case 3: black pawn placement and white promotion 
                                                    else if ((numOnBoard WK (theBoard updatedGS) == 2) && (numOnBoard BK (theBoard updatedGS) < 2)) then
                                                        do
                                                            let updatedGS' = promotePawn updatedGS (theBoard updatedGS) (findPawnPromote (last (theBoard updatedGS)) False True 0) 
                                                            blackMove <- black updatedGS' PawnPlacement Black                                  
                                                            let updatedGS'' = do
                                                                case blackMove of
                                                                    Nothing -> updatePP updatedGS' Nothing Nothing True False
                                                                    maybe -> updatePP updatedGS' (Just [(findPawn (theBoard updatedGS) True (0,0)), (head $ fromJust blackMove)]) (getPawnPlace (whitePlay updatedGS')) True False
                                                            let finalUpdate = GameState (blackPlay updatedGS'') (blackPen updatedGS'') (whitePlay updatedGS') (whitePen updatedGS') (theBoard updatedGS'')  
                                                            gameLoop updatedGS'' black blackStr white whiteStr Normal False
                                                    -- * case 4: two pawn placements needed
                                                    else
                                                        do
                                                            blackMove <- black currentBoard PawnPlacement Black
                                                            whiteMove <- white currentBoard PawnPlacement White                                                    
                                                            let updatedGS' = do
                                                                case blackMove of
                                                                    Nothing -> updatePP updatedGS Nothing Nothing True False
                                                                    maybe -> updatePP updatedGS (Just [(findPawn (theBoard updatedGS) True (0,0)), (head $ fromJust blackMove)]) Nothing True False
                                                            let updatedGS'' = do
                                                                case whiteMove of
                                                                    Nothing -> updatePP updatedGS' (getPawnPlace (blackPlay updatedGS')) Nothing False True
                                                                    maybe -> updatePP updatedGS' (getPawnPlace (blackPlay updatedGS')) (Just [(findPawnPromote (last (theBoard updatedGS')) False True 0), (head $ fromJust whiteMove)]) False True
                                                            let finalUpdate = GameState (blackPlay updatedGS') (blackPen updatedGS') (whitePlay updatedGS'') (whitePen updatedGS'') (theBoard updatedGS'')  
                                                            gameLoop finalUpdate black blackStr white whiteStr Normal False
                                            else if ((numInRow BP $ (theBoard updatedGS)!!0) > 0) && ((numOnBoard BK (theBoard updatedGS) < 2)) then
                                                do
                                                    let updatedGS' = promotePawn updatedGS (theBoard updatedGS) (findPawnPromote (head (theBoard updatedGS)) True False 0)
                                                    gameLoop updatedGS' black blackStr white whiteStr Normal False
                                            else if ((numInRow BP $ (theBoard updatedGS)!!0) > 0) && ((numOnBoard BK (theBoard updatedGS) == 2)) then
                                                do
                                                    blackMove <- black currentBoard PawnPlacement Black
                                                    let updatedGS' = do
                                                        case blackMove of
                                                            Nothing -> updatePP updatedGS Nothing Nothing True False
                                                            maybe -> updatePP updatedGS (Just [(findPawn (theBoard updatedGS) True (0,0)), (head $ fromJust blackMove)]) Nothing True False
                                                    gameLoop updatedGS' black blackStr white whiteStr Normal False
                                            else if ((numInRow WP $ (theBoard updatedGS)!!4) > 0) && ((numOnBoard WK (theBoard updatedGS) < 2)) then
                                                do
                                                    let updatedGS' = promotePawn updatedGS (theBoard updatedGS) (findPawnPromote (last (theBoard updatedGS)) False True 0)
                                                    gameLoop updatedGS' black blackStr white whiteStr Normal False
                                            else if ((numInRow WP $ (theBoard updatedGS)!!4) > 0) && ((numOnBoard WK (theBoard updatedGS) == 2)) then
                                                do
                                                    whiteMove <- white currentBoard PawnPlacement White
                                                    let updatedGS' = do
                                                        case whiteMove of
                                                            Nothing -> updatePP updatedGS Nothing Nothing False True
                                                            maybe -> updatePP updatedGS Nothing (Just [(findPawnPromote (last (theBoard updatedGS)) False True 0), (head $ fromJust whiteMove)]) False True
                                                    gameLoop updatedGS' black blackStr white whiteStr Normal False 
                                            else return()
                                    else gameLoop updatedGS black blackStr white whiteStr Normal False
                                   
            else
                do 
                    let gameOverMsg = getGameOverMsg blackStr ((length $ getPieces (theBoard currentBoard) Black PawnPlacement)) (blackPen currentBoard) whiteStr ((length $ getPieces (theBoard currentBoard) White PawnPlacement)) (whitePen currentBoard)
                    putStrLn gameOverMsg

    -- | end == True
    --   in this case, find the winners and print out the game over message
    --   draw conditions
    else 
        do 
            let gameOverMsg = getGameOverMsg blackStr ((length $ getPieces (theBoard currentBoard) Black PawnPlacement)) (blackPen currentBoard) whiteStr ((length $ getPieces (theBoard currentBoard) White PawnPlacement)) (whitePen currentBoard)
            putStrLn gameOverMsg

notEnd :: GameState -> Bool
notEnd currentGame = let findBlackPawns = findPawns (theBoard currentGame) Black
                         findWhitePawns = findPawns (theBoard currentGame) White
                         -- check if players accumulated >2 penalty points
                         blackMaxPenalty = blackPen currentGame >= 2
                         whiteMaxPenalty = whitePen currentGame >= 2
                    in ((findBlackPawns == True && findWhitePawns == True) && (blackMaxPenalty == False && whiteMaxPenalty == False))

getGameOverMsg :: String -> Int -> Int -> String -> Int -> Int -> String
getGameOverMsg black blackPawns blackPenalty white whitePawns whitePenalty
    | (blackPawns == whitePawns) && ((blackPenalty == whitePenalty) || (blackPenalty < 2) || (whitePenalty < 2)) = formatGOMsg "Game is a Draw!" black blackPawns white whitePawns
    | (blackPenalty > 1) = formatGOMsg "White wins! (Because Black accumulated >1 penalty points.)" black blackPawns white whitePawns
    | (whitePenalty > 1) = formatGOMsg "Black wins! (Because White accumulated >1 penalty points.)" black blackPawns white whitePawns
    | (blackPawns == 0) || (blackPawns < whitePawns) = formatGOMsg "White wins!" black blackPawns white whitePawns
    | (whitePawns == 0) || (blackPawns > whitePawns) = formatGOMsg "Black wins!" black blackPawns white whitePawns


formatGOMsg :: String -> String -> Int -> String -> Int -> String
formatGOMsg winMsg blackStr blackPawns whiteStr whitePawns = winMsg ++ "  Black (" ++ blackStr ++ "): " ++ (show blackPawns) ++ "  White (" ++ whiteStr ++"): " ++ (show whitePawns)

---Gamestate updating functions-----------------------------------------------------

getPawnPlace :: Played -> Maybe([(Int, Int)])
getPawnPlace (PlacedPawn move) = Just [fst move, snd move]
--getPawnPlace (UpgradedPawn2Knight move) = Just [move]
getPawnPlace _ = Nothing

-- | updateGS: updates the game state based on the players' inputted moves
--   params: the game state to update, current play type (normal/pawn placement), black player's move, white player's move
updateGS :: GameState -> PlayType -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> GameState
updateGS toUpdate Normal blackMove whiteMove = do
    -- * check for validity 
    let blackValid = do
        case blackMove of
            Nothing -> True 
            maybe -> checkMoveLegal (theBoard toUpdate) Black (head $ fromJust blackMove) (last $ fromJust blackMove)
    let whiteValid = do
        case whiteMove of
            Nothing -> True 
            maybe -> checkMoveLegal (theBoard toUpdate) White (head $ fromJust whiteMove) (last $ fromJust whiteMove)
    -- * values to print in the updated and printed game board
    let blackPlayType = do
        case blackValid of
            True -> if (blackMove == Nothing) then Passed else Played (head $ fromJust blackMove, last $ fromJust blackMove)
            False -> Goofed (head $ fromJust blackMove, last $ fromJust blackMove)
    let whitePlayType = do
        case whiteValid of
            True -> if (whiteMove == Nothing) then Passed else Played (head $ fromJust whiteMove, last $ fromJust whiteMove)
            False -> Goofed (head $ fromJust whiteMove, last $ fromJust whiteMove)
    -- * updates the penalty points
    let blackNP = do
        case blackValid of
            True -> blackPen toUpdate
            False -> (blackPen toUpdate) + 1
    let whiteNP = do
        case whiteValid of
            True -> whitePen toUpdate
            False -> (whitePen toUpdate) + 1
    -- | updates the game state based on these updated values, also updating the board if the user does not goof up
    -- * case 1: the both players goofed on the same round
    if ((whitePlayType == Goofed (head $ fromJust whiteMove, last $ fromJust whiteMove)) && (blackPlayType == Goofed (head $ fromJust blackMove, last $ fromJust blackMove))) then
        GameState blackPlayType blackNP whitePlayType whiteNP (theBoard toUpdate)  
    -- * case 2 & 3: either the black player or the white player goof on a round
    else if (blackPlayType == Goofed (head $ fromJust blackMove, last $ fromJust blackMove)) then
        GameState blackPlayType blackNP whitePlayType whiteNP (updateBoard (theBoard toUpdate) Normal Passed whitePlayType)
    else if (whitePlayType == Goofed (head $ fromJust whiteMove, last $ fromJust whiteMove)) then
        GameState blackPlayType blackNP whitePlayType whiteNP (updateBoard (theBoard toUpdate) Normal blackPlayType Passed)  
    -- * otherwise, proceed as a normal game 
    else
        GameState blackPlayType blackNP whitePlayType whiteNP (updateBoard (theBoard toUpdate) Normal blackPlayType whitePlayType)

findPawnPromote :: [Cell] -> Bool -> Bool -> Int -> (Int, Int)
findPawnPromote (x:xs) True white startCoord
    | (x == BP) = (startCoord, 0)
    | otherwise = findPawnPromote xs True white (startCoord + 1)
findPawnPromote (x:xs) black True startCoord
    | (x == WP) = (startCoord, 4)
    | otherwise = findPawnPromote xs black True (startCoord + 1)


promotePawn :: GameState -> Board -> (Int, Int) -> GameState
promotePawn toUpdate board coord
    | ((getFromBoard board coord) == BP) = do
        let newBoard = replace2 board coord BK
        GameState (UpgradedPawn2Knight coord) (blackPen toUpdate) (None) (whitePen toUpdate) newBoard
    | otherwise = do
        let newBoard = replace2 board coord WK
        GameState (None) (blackPen toUpdate) (UpgradedPawn2Knight coord) (whitePen toUpdate) newBoard 
-- TODO: pawn placement cases
-- updateGS toUpdate PawnPlacement blackMove whiteMove = do

updatePP :: GameState -> Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> Bool -> Bool -> GameState 
updatePP toUpdate blackMove whiteMove isBlack isWhite = do
    let blackValid = do
        case blackMove of
            Nothing -> if (isBlack == True) then False else True 
            maybe -> checkEmptySpace (theBoard toUpdate) (last $ fromJust blackMove)
    let whiteValid = do
        case whiteMove of
            Nothing -> if (isWhite == True) then False else True  
            maybe -> checkEmptySpace (theBoard toUpdate) (last $ fromJust whiteMove)
    let blackPlayType = do
        case blackValid of
            True ->  if (isBlack == True )then  PlacedPawn (head $ fromJust blackMove, last $ fromJust blackMove) else None
            False -> do 
                case blackMove of 
                    Nothing -> NullPlacedPawn
                    maybe -> BadPlacedPawn (head $ fromJust blackMove, last $ fromJust blackMove)
    let whitePlayType = do
        case whiteValid of
            True ->  if (isWhite == True) then PlacedPawn (head $ fromJust whiteMove, last $ fromJust whiteMove)  else None
            False -> do
                case blackMove of 
                    Nothing ->  NullPlacedPawn
                    maybe -> BadPlacedPawn (head $ fromJust whiteMove, last $ fromJust whiteMove)
    let blackNP = do
        case blackValid of
            True -> blackPen toUpdate
            False -> (blackPen toUpdate) +  1 
    let whiteNP = do
        case whiteValid of
            True -> whitePen toUpdate
            False -> (whitePen toUpdate) +  1 
            
    if (blackPlayType == NullPlacedPawn) && (isBlack == True) then
        -- white should be (none, pp)
       GameState blackPlayType blackNP whitePlayType whiteNP (theBoard toUpdate)  
            -- * case 2 & 3: either the black player or the white player goof on a round
    else if (whitePlayType == NullPlacedPawn) && (isWhite == True) then
       -- black should be (none, pp)
       GameState blackPlayType blackNP whitePlayType whiteNP (theBoard toUpdate) 
            -- * otherwise, proceed as a normal game 
    else 
       GameState blackPlayType blackNP whitePlayType whiteNP (updateBoard (theBoard toUpdate) PawnPlacement blackPlayType whitePlayType)


-- | updateBoard updates the board based on the current play type (normal/pawn placement)
--   and the player's played type (played/passed/etc)
--   params: the game board, current play type, black player's played type, white player's played type
updateBoard :: Board -> PlayType -> Played -> Played -> Board -- use IO (Maybe [(Int,Int)]) in case Played doesnt work
-- * normal play type cases
updateBoard toUpdate Normal Passed (Played whiteMove) = update1Player toUpdate whiteMove
updateBoard toUpdate Normal (Played blackMove) Passed = update1Player toUpdate blackMove
updateBoard toUpdate Normal (Played blackMove) (Played whiteMove) = update2Players toUpdate blackMove whiteMove
-- * TODO: pawn placement play type cases
updateBoard toUpdate PawnPlacement (PlacedPawn blackMove) (PlacedPawn whiteMove) = update2Players toUpdate blackMove whiteMove
updateBoard toUpdate PawnPlacement (PlacedPawn blackMove) _ = update1Player toUpdate blackMove
updateBoard toUpdate PawnPlacement _ (PlacedPawn whiteMove) = update1Player toUpdate whiteMove
updateBoard toUpdate PawnPlacement _ _ = toUpdate

-- | update1Player: updates the game board when only one player is making a move
--   params: the game board, current play type (normal/pawn placement), player's move
update1Player :: Board -> ((Int, Int), (Int, Int)) -> Board
update1Player toUpdate playerMove = do
    let tmp = updateDestCell toUpdate (fst playerMove) (snd playerMove) -- ^ temporary game state with new updated destinations set
    clearCell tmp (fst playerMove)
    


-- | update2Players: updates the board based on the two player's inputted moves
--   params: the game board, current play type, black plater's move, white player's move
update2Players :: Board -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Board
update2Players toUpdate blackMove whiteMove = do
    -- * when both players collide, call the updateDestCell' function and clear the starting points after
    if ((snd blackMove) == (snd whiteMove)) then
        do
            let tmp = updateDestCell' toUpdate blackMove whiteMove   -- ^ updates the destination coordinates
            let tmp2 = clearCell tmp (fst blackMove)                 -- ^ clears the players' starting coordinates
            clearCell tmp2 (fst whiteMove)
    -- * otherwise, just call updateDestCell'
    else updateDestCell' toUpdate blackMove whiteMove

-- | clearCell: replaces the target coordinate into an empty cell in the board
--   params: game board, coordinate to empty
clearCell :: Board -> (Int, Int) -> Board
clearCell board cell = replace2 board cell E

-- | updateDestCell: updates game board with one destination coordinate 
--   this happens when either only one player is making a move or two players are moving to separation destinations (no collisions)
--   params: the game board, a player's start coordinates, and a player's destination coordinates
updateDestCell :: Board -> (Int, Int) -> (Int, Int) -> Board
updateDestCell toUpdate start dest
    -- * this is when one player "captures" an opponent piece
    | ((startCell == WK || startCell == WP) && (destCell == BK || destCell == BP)) || ((startCell == BK || startCell == BP) && (destCell == WK || destCell == WP)) = replace2 toUpdate dest startCell
    -- * player moves to an empty spot
    | otherwise = replace2 toUpdate dest startCell
    where startCell = getFromBoard toUpdate start
          destCell  = getFromBoard toUpdate dest

-- | updateDestCell': updates the game board with two destination coordinates
--   this is the case where players are moving pieces at the same time
--   params: the game board, the black player's move, the white player's move
updateDestCell' :: Board -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Board
updateDestCell' toUpdate blackMove whiteMove
    -- * this is the case when both players have the same destination and are moving the same type of piece (pawn vs pawn or knight vs knight)
    | (whiteDest == blackDest) && ((whiteSC == WK && blackSC == BK) || (whiteSC == WP && blackSC == BP)) = clearCell toUpdate whiteDest
    -- * the following two cases are when the destinations are the same, but it's a knight vs pawn
    | (whiteDest == blackDest) && (whiteSC == WK && blackSC == BP) = replace2 toUpdate whiteDest whiteSC
    | (whiteDest == blackDest) && (whiteSC == WP && blackSC == BK) = replace2 toUpdate whiteDest blackSC
    -- * this is the case when the pieces are "swapped" together (both tried to eat each other)
    | (whiteSrc == blackDest) && (blackSrc == whiteDest) = do
        let tmp = replace2 toUpdate whiteDest whiteSC
        replace2 tmp blackDest blackSC
    -- * this is the case when the black player's destination is the same as the moving white pawn's starting coordinates
    | (whiteSrc == blackDest) = do
        let tmp = replace2 toUpdate whiteDest whiteSC
        let tmp2 = clearCell tmp whiteSrc
        let tmp3 = replace2 tmp2 blackDest blackSC
        clearCell tmp3 blackSrc
    -- * otherwise, both players are moving to an empty space or to an unmoved opponent, so updateDestCell can be used
    | otherwise = do
        let tmp = updateDestCell toUpdate blackSrc blackDest  -- ^ updates the destination cells as non-collisions 
        let tmp2 = clearCell tmp blackSrc
        let tmp3 = updateDestCell tmp2 whiteSrc whiteDest
        clearCell tmp3 whiteSrc
    where whiteSrc = fst whiteMove                                   -- ^ start coordinates
          blackSrc = fst blackMove
          whiteDest = snd whiteMove                                  -- ^ destination coordinates
          blackDest = snd blackMove
          whiteSC = getFromBoard toUpdate $ fst whiteMove            -- ^ cells of players' stating coordinates
          blackSC = getFromBoard toUpdate $ fst blackMove
          whiteDC = getFromBoard toUpdate whiteDest                  -- ^ cells of players' destination coordinates
          blackDC = getFromBoard toUpdate blackDest

-- CHECK PIECES FOR PAWNPLACEMENT--
--CHECK PAWNS IN ROWS ---

-- | The number of a certain Cell in a list of cells.
numInRow :: Cell -> [Cell] -> Int 
numInRow _ [] = 0
numInRow cell (x:xs) 
   | cell == x = 1 + numInRow cell xs
   | otherwise = numInRow cell xs

-- | The number of a certain Cell on the board. e.g. the number of black pawns.
numOnBoard :: Cell -> Board -> Int
numOnBoard cell (xs:[]) = numInRow cell xs
numOnBoard cell (xs:xss) = numInRow cell xs + numOnBoard cell xss  

findPawn :: Board -> Bool -> (Int, Int) -> (Int, Int)
findPawn board True (x,y) | (getFromBoard board (x,y) == BP) = (x,y)
                          | otherwise = findPawn board True (x+1,y)
findPawn board False (x,y) | (getFromBoard board (x,y) == WP) = (x,y)
                           | otherwise = findPawn board False (x+1,y)

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
