module State.Player where

import           Util.Positioning
import           Util.Color
import           Util.Config

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

-- bleeds color from the player
bleed :: Float -> Player -> Player
bleed s p@Player { colors = c } =
  let s' = s * playerBleedRate in setColor (subCmyk c (s', s', s', s')) p

-- barfs the current colors from the player if
-- possible
barf :: Player -> (Bool, Player)
barf pl@Player { colors = (c, m, y, k) } =
  let cmyk@[c', m', y', k'] = map (subtract playerBarfThreshold) [c, m, y, k]
      can                   = all (> 0) cmyk
  in  (can, if can then setColor (c', m', y', k') pl else pl)
