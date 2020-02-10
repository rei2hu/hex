module State.Player where

import Util.Positioning (OffsetCoords)
data Player = Player OffsetCoords deriving Show

moveX :: Int -> Player -> Player
moveX n (Player (x, y)) = Player ((x + n), y)

moveY :: Int -> Player -> Player
moveY n (Player (x, y)) = Player (x, (y + n))

positionOf :: Player -> OffsetCoords
positionOf (Player coords) = coords
