module Main
  ( main
  ) where

import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Logic
import Minimax
import Rendering

main :: IO ()
main = play window background fps emptyGame renderGame handleMouse (const id)

fps :: Int
fps = 60

window :: Display
window = InWindow "" (round gameWidth, round gameHeight) (200, 200)

background :: Color
background = makeColorI 40 42 54 0

handleMouse :: Event -> Game -> Game
handleMouse (EventKey (MouseButton LeftButton) Down _ (x, y)) game
  | gamePlayer game == PlayerX = minimax $ playMove game targetCell
  | otherwise = minimax game
  where
    targetCellPos =
      floor (normalizedX / cellSize) + n * floor (normalizedY / cellSize)
    targetCell = gameBoard game !! targetCellPos
    normalizedX = (gameWidth / 2) + x
    normalizedY = (gameHeight / 2) - y
handleMouse _ game = game
