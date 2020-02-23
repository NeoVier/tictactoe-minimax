module Logic
  ( playMove
  , checkEnd
  ) where

import Data.Maybe (isJust)
import Game

playMove :: Game -> Cell -> Game
playMove game cell
  | gameState game == Running = newGame
  | otherwise = game
  where
    newGame = checkEnd $ updateCell game (gameBoard game !! snd cell)

checkEnd :: Game -> Game
checkEnd game
  | isJust checkLine = game {gameState = GameOver checkLine}
  | isJust checkCol = game {gameState = GameOver checkCol}
  | isJust checkDiag = game {gameState = GameOver checkDiag}
  | checkFull = game {gameState = GameOver Nothing}
  | otherwise = game
  where
    checkLine = lineEnd (gameBoard game)
    checkCol = colEnd (gameBoard game)
    checkDiag = diagEnd (gameBoard game)
    checkFull = all ((/= Nothing) . fst) (gameBoard game)

boardMatrix :: Board -> [[Cell]]
boardMatrix board = [start, middle, end]
  where
    (start, midEnd) = splitAt n board
    (middle, end) = splitAt n midEnd

transpose :: [[a]] -> [[a]]
transpose matrix = map (getLine matrix) [0 .. length matrix - 1]
  where
    getLine m i = map (!! i) m

lineEnd :: Board -> Maybe Player
lineEnd board
  | null result = Nothing
  | otherwise = head result
  where
    checkList = map check (boardMatrix board)
    result = dropWhile (== Nothing) checkList

colEnd :: Board -> Maybe Player
colEnd board = lineEnd transposed
  where
    transposed = concat $ transpose (boardMatrix board)

diagEnd :: Board -> Maybe Player
diagEnd board
  | isJust mainResult = mainResult
  | isJust secondResult = secondResult
  | otherwise = Nothing
  where
    matrix = boardMatrix board
    mainDiag = zipWith (!!) matrix [0 .. length matrix - 1]
    mainResult = check mainDiag
    secondDiag =
      zipWith
        (!!)
        (boardMatrix board)
        [length matrix - 1,length matrix - 2 .. 0]
    secondResult = check secondDiag

check :: [Cell] -> Maybe Player
check section
  | any ((/= (fst $ head section)) . fst) section = Nothing
  | otherwise = fst $ head section
