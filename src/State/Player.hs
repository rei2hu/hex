module State.Player where

import Util.Positioning (OffsetCoords)
data Player = Player { pos :: OffsetCoords } deriving Show

-- some player constructor (necessary?)
player :: OffsetCoords -> Player
player p = Player { pos = p }

setPos :: Player -> OffsetCoords -> Player
setPos _ c = Player { pos = c }

-- moves the player in the x direction
moveX :: Player -> Int -> Player
moveX p n = let (x, y) = pos p
            in setPos p (x + n, y)

-- moves the player in the y direction
moveY :: Player -> Int -> Player
moveY p n = let (x, y) = pos p
            in setPos p (x, y + n)
