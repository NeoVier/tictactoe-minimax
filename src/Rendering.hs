module Rendering
  ( renderGame
  ) where

import Game
import Graphics.Gloss

type Grid = Picture

padding :: Float
padding = 20

gridColor :: Color
gridColor = makeColorI 80 250 123 255

transparent :: Color
transparent = makeColor 0 0 0 0

playerColor :: Maybe Player -> Color
playerColor (Just PlayerX) = makeColorI 255 85 85 255
playerColor (Just PlayerO) = makeColorI 255 184 108 255
playerColor Nothing = makeColorI 98 114 164 255

playerShape :: Player -> Color -> Picture
playerShape PlayerX clr = color resultingColor $ pictures [leftR, rightR]
  where
    leftR = rotate 135 rect
    rightR = rotate 45 rect
    rect = rectangleSolid (cellSize - padding) 10
    resultingColor
      | clr == transparent = playerColor (Just PlayerX)
      | otherwise = clr
playerShape PlayerO clr =
  color resultingColor $ thickCircle ((cellSize / 2) - padding) 10
  where
    resultingColor
      | clr == transparent = playerColor (Just PlayerO)
      | otherwise = clr

emptyGrid :: Grid
emptyGrid = pictures (map (color gridColor) [horizontalLines, verticalLines])
  where
    horizontalLines = rectangleWire (gameWidth + 5) (gameHeight / floatN)
    verticalLines = rectangleWire (gameWidth / floatN) (gameHeight + 5)

renderGame :: Game -> Picture
renderGame game =
  pictures $ emptyGrid : map (renderCell desiredColor) (gameBoard game)
  where
    desiredColor = renderColor (gameState game)

renderColor :: State -> Color
renderColor Running = transparent
renderColor (GameOver player) = playerColor player

translateToPos :: Position -> Picture -> Picture
translateToPos pos =
  translate
    (cellSize * realToFrac ((pos `mod` n) - 1))
    (cellSize * realToFrac (1 - (pos `div` n)))

renderCell :: Color -> Cell -> Picture
renderCell _ (Nothing, _) = blank
renderCell clr (Just player, pos) = translateToPos pos (playerShape player clr)
