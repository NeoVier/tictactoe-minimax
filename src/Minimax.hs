module Minimax where

import Data.List
import Data.Maybe (isJust)
import Data.Ord
import Game
import Logic

winValue :: State -> Float
winValue (GameOver (Just PlayerX)) = -1
winValue (GameOver (Just PlayerO)) = 1
winValue _ = 0

minimax :: Game -> Game
minimax game
  | endedGame = game
  | otherwise = fst $ compareFunc (comparing snd) $ zip games scores
  where
    endedGame = gameState game /= Running
    compareFunc
      | gamePlayer game == PlayerX = minimumBy
      | otherwise = maximumBy
    games = possibleMoves game
    scores = map score games

score :: Game -> Float
score game
  | endedGame = winValue (gameState game)
  | otherwise = scoreFunc (map score (possibleMoves game))
  where
    endedGame = gameState game /= Running
    scoreFunc
      | gamePlayer game == PlayerX = minimum
      | otherwise = maximum

possibleMoves :: Game -> [Game]
possibleMoves game = filter (/= game) (map simulateMove [0 .. n * n - 1])
  where
    desiredCell pos = gameBoard game !! pos
    simulateMove pos
      | isJust $ fst (desiredCell pos) = game
      | otherwise = playMove game (desiredCell pos)
