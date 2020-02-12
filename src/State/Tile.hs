module State.Tile where

import Graphics.Gloss.Data.Color
import Util.Positioning
import Util.Color
import Data.Maybe
import Data.Map.Strict
import System.Random

type TileMap = Map OffsetCoords Tile
data Tile = Tile { pos :: OffsetCoords, colors :: Cmyk } deriving Show

-- some tile constructor (is this even necessary?)
tile :: OffsetCoords -> Cmyk -> Tile
tile p c = Tile { pos = p, colors = c }

-- makes a tile at a position
makeTileAt :: TileMap -> OffsetCoords -> [Float] -> TileMap
makeTileAt ts p [c, m, y, k] = insert p (tile p (c, m, y, k)) ts

-- find the tile at a position
-- creates one if it doesnt exist (does this have a use?)
tileAt :: TileMap -> OffsetCoords -> [Float] -> TileMap
tileAt ts p s = case Data.Map.Strict.lookup p ts of
                Nothing -> makeTileAt ts p s
                (Just _) -> ts

-- gets the neighbors of a position
-- 1 is the one directly north then rest are in
-- clockwise rotation
nghbr :: TileMap -> OffsetCoords -> Int -> Maybe Tile
nghbr t (x, y) 1 = t !? (x, y - 1)
nghbr t (x, y) 2 = t !? (x + 1, y) 
nghbr t (x, y) 3 = t !? (x + 1, y - 1) 
nghbr t (x, y) 4 = t !? (x + 1, y - 1) 
nghbr t (x, y) 5 = t !? (x, y + 1) 
nghbr t (x, y) 6 = t !? (x - 1, y + 1) 
nghbr _ _ _ = error "invalid neighbor"

-- determines one of the neighbors at a position
-- is "dark". If the tile hasn't been generated
-- yet it is also considered dark
hasDarkNghbr :: TileMap -> Tile -> Bool
hasDarkNghbr ts (Tile { pos = p }) = let pred (Just (Tile { colors = (_, _, _, k) })) = k > 0.5
                                         pred Nothing = True
                                     in any pred $ fmap (nghbr ts p) [1..6]

-- sets the color of a tile
setColor :: Tile -> Color -> Tile
setColor (Tile { pos = p }) c = tile p $ colorToCmyk c

-- darkens a tile by increasing its "key" value
darken :: Tile -> Float -> Tile
darken (Tile { pos = p, colors = (c, m, y, k) }) s = tile p (c, m, y, min 1 (k + 0.1 * s))

-- lightens a tile by decreasing its "key" value
lighten :: Tile -> Float -> Tile
lighten (Tile { pos = p, colors = (c, m, y, k) }) s = tile p (c, m, y, max 0 (k - 0.1 * s))
