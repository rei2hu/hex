module State.Player where

import           Util.Positioning
import           Util.Color

data Player = Player { pos :: OffsetCoords, colors :: Cmyk } deriving Show

-- some player constructor
player :: Player
player = Player { pos = (0, 0), colors = (0.5, 0.5, 0.5, 0) }

-- sets the color of a player
setColor :: Cmyk -> Player -> Player
setColor cl Player { pos = p } = Player p cl

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
addColor :: Cmyk -> Player -> Player
addColor c Player { pos = p, colors = c' } =
  Player { pos = p, colors = addCmyk c c' }

bleed :: Float -> Player -> Player
bleed s p@Player { colors = c } =
  let s' = s * 0.1 in setColor (subCmyk c (s', s', s', s')) p
