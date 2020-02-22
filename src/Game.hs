module Game where

import Graphics.Gloss.Interface.IO.Interact

data Player
  = PlayerX
  | PlayerO
  deriving (Eq, Show)

type Position = Int

type Cell = (Maybe Player, Position)

data State
  = Running
  | GameOver (Maybe Player)
  deriving (Eq, Show)

type Board = [Cell]

n :: Int
n = 3

floatN :: Float
floatN = fromIntegral n

gameWidth :: Float
gameWidth = 600

gameHeight :: Float
gameHeight = gameWidth

cellSize :: Float
cellSize = gameWidth / floatN

data Game =
  Game
    { gameBoard :: Board
    , gameState :: State
    , gamePlayer :: Player
    }
  deriving (Show)

emptyBoard :: Board
emptyBoard = zip (repeat Nothing) [0 .. 8]

emptyGame :: Game
emptyGame = Game emptyBoard Running PlayerX

updateCell :: Game -> Cell -> Game
updateCell game (Just player, _) = game
updateCell game (Nothing, pos) =
  game {gameBoard = newBoard, gamePlayer = newPlayer}
  where
    newCell = (Just (gamePlayer game), pos)
    newBoard = replace (gameBoard game) pos newCell
    newPlayer
      | gamePlayer game == PlayerX = PlayerO
      | otherwise = PlayerX

replace :: [a] -> Int -> a -> [a]
replace xs pos val = start ++ val : tail end
  where
    (start, end) = splitAt pos xs
