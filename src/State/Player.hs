module State.Player where

import           Util.Positioning
import           Util.Color

data Player = Player { pos :: OffsetCoords, colors :: Cmyk } deriving Show

-- some player constructor (necessary?)
player :: Player
player = Player { pos = (0, 0), colors = (1, 1, 1, 0) }

setPos :: Player -> OffsetCoords -> Player
setPos Player { colors = cl } c = Player { pos = c, colors = cl }

-- moves the player in the x direction
moveX :: Player -> Int -> Player
moveX p n = let (x, y) = pos p in setPos p (x + n, y)

-- moves the player in the y direction
moveY :: Player -> Int -> Player
moveY p n = let (x, y) = pos p in setPos p (x, y + n)

modifyColor :: Player -> (Float, Float, Float) -> Player
modifyColor Player { pos = p, colors = (c, m, y, k) } (c', m', y') =
  Player { pos = p, colors = (c + c', m + m', y + y', k) }
