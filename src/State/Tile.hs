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

-- sets the color of a tile
setColor :: Tile -> Cmyk -> Tile
setColor (Tile { pos = p }) c = Tile { pos = p, colors = c }

-- makes a tile at a position
makeTileAt :: TileMap -> OffsetCoords -> [Float] -> TileMap
makeTileAt ts p [c, m, y, k] = insert p (tile p (c, m, y, k)) ts

-- find the tile at a position
-- creates one if it doesnt exist (does this have a use?)
tileAt :: TileMap -> OffsetCoords -> [Float] -> TileMap
tileAt ts p s = case Data.Map.Strict.lookup p ts of
                Nothing -> makeTileAt ts p s
                (Just _) -> ts

getTileAt :: TileMap -> OffsetCoords -> Tile
getTileAt ts p = case ts !? p of
                 (Just a) -> a
                 Nothing -> error $ "tried to access nonexistant tile at " ++ (show p)

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
hasDarkNghbr ts = (> 0) . numDarkNghbrs ts

-- the number of dark neighbors a tile has
-- TODO: use for scaling darken value with
-- Int -> (a -> a) -> (a -> a) aka (nTimes)
numDarkNghbrs :: TileMap -> Tile -> Int
numDarkNghbrs ts t = let pred (Just (Tile { colors = (_, _, _, k) })) = k > 0.5
                         pred Nothing = True
                     in length . Prelude.filter pred $ fmap (nghbr ts $ pos t) [1..6]

-- darkens a tile by increasing its "key" value
darken :: Tile -> Float -> Tile
darken t s = let (c, m, y, k) = colors t
             in setColor t (c, m, y, min 1 (k + 0.1 * s))

-- lightens a tile by decreasing its "key" value
lighten :: Tile -> Float -> Tile
lighten t s = let (c, m, y, k) = colors t
              in setColor t (c, m, y, max 0 (k - 0.5 * s))
