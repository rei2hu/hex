module State.Player where

import           Util.Positioning
import           Util.Color

data Player = Player { pos :: OffsetCoords, colors :: Cmyk } deriving Show

-- some player constructor
player :: Player
player = Player { pos = (0, 0), colors = (0.5, 0.5, 0.5, 0) }

-- sets the position of a player
setPos :: OffsetCoords -> Player -> Player
setPos p Player { colors = cl } = Player p cl

-- moves the player in the x direction
moveX :: Int -> Player -> Player
moveX n pl = let (x, y) = pos pl in setPos (x + n, y) pl

-- moves the player in the y direction
moveY :: Int -> Player -> Player
moveY n pl = let (x, y) = pos pl in setPos (x, y + n) pl

-- modifies the color of the player by
-- adding the provided color to it
modifyColor :: Cmyk -> Player -> Player
modifyColor c Player { pos = p, colors = c' } =
  Player { pos = p, colors = addCmyk c c' }
