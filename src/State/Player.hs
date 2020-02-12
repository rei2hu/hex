module State.Player where

import Util.Positioning (OffsetCoords)
data Player = Player { pos :: OffsetCoords } deriving Show

-- some player constructor (necessary?)
player :: OffsetCoords -> Player
player p = Player { pos = p }

-- moves the player in the x direction
moveX :: Player -> Int -> Player
moveX (Player { pos = (x, y) }) n = player (x + n, y)

-- moves the player in the y direction
moveY :: Player -> Int -> Player
moveY (Player { pos = (x, y) }) n = player (x, y + n)
