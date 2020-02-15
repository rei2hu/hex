module State.Tile where

import           Util.Positioning
import           Util.Color
import           Util.Config
import           Data.Map.Strict

type TileMap = Map OffsetCoords Tile
data Tile = Tile { pos :: OffsetCoords, colors :: Cmyk } deriving Show
data Neighbor = N | NE | SE | S | SW | NW deriving Enum

-- basic rules:
-- if tile is dark
--   then cannot bleed
-- else if tile is finished bleeding
--   then cannot be darkened
--
-- what prevents camping on a tile until bled?
-- cant bleed if no space
-- darkness is faster than bleed

-- sets the color of a tile
setColor :: Cmyk -> Tile -> Tile
setColor c t = Tile { pos = pos t, colors = c }

-- determines if a tile is dark by random criteria
isDark :: Tile -> Bool
isDark Tile { colors = (_, _, _, k) } = k > tileDarkThreshold

-- darkens a tile by increasing its "key" value
darken :: Float -> Tile -> Tile
darken s t =
  let (c, m, y, k) = colors t
  in  case (c, m, y, k) of
        (0, 0, 0, _) -> lighten s t
        _            -> setColor (c, m, y, min 1 (k + tileDarkenRate * s)) t

-- lightens a tile by decreasing its "key" value
lighten :: Float -> Tile -> Tile
lighten s t =
  let (c, m, y, k) = colors t
  in  setColor (c, m, y, max 0 (k - tileLightenRate * s)) t

-- inserts a tile into a tilemap (replaces if already exists)
replace :: Tile -> TileMap -> TileMap
replace t = insert (pos t) t

-- bleeds the tile's colors a certain amount
bleed :: Float -> Tile -> (Cmyk, Tile)
bleed s t@Tile { colors = c } = case c of
  (_, _, _, k) | k > tileBleedThreshold -> ((0, 0, 0, 0), t)
  _ ->
    let s'                  = s
        c'@(cc, cm, cy, ck) = subCmyk c (s', s', s', 0)
    in  ((min s' cc, min s' cm, min s' cy, min s' ck), setColor c' t)

--
-- TileMap functions
--

-- makes a tile at a position
makeTileAt :: [Float] -> OffsetCoords -> TileMap -> TileMap
makeTileAt [c, m, y, k] p ts = insert p (Tile p (c, m, y, k)) ts
makeTileAt _            _ _  = error "used makeTileAt wrong"

-- makes a tile at a position only if it doesn't already exist
mmakeTileAt :: [Float] -> OffsetCoords -> TileMap -> TileMap
mmakeTileAt s p ts = case ts !? p of
  Nothing  -> makeTileAt s p ts
  (Just _) -> ts

-- gets a tile at a position, only use when guaranteed
getTileAt :: OffsetCoords -> TileMap -> Tile
getTileAt p ts = case ts !? p of
  (Just a) -> a
  Nothing  -> error $ "tried to access nonexistant tile at " ++ show p

-- gets the neighbor of a position
nghbr :: Neighbor -> OffsetCoords -> TileMap -> Maybe Tile
nghbr N  (x, y) t = t !? (x, y - 1)
nghbr NE (x, y) t = t !? (x + 1, y)
nghbr SE (x, y) t = t !? (x + 1, y - 1)
nghbr S  (x, y) t = t !? (x + 1, y - 1)
nghbr SW (x, y) t = t !? (x, y + 1)
nghbr NW (x, y) t = t !? (x - 1, y + 1)

-- determines one of the neighbors at a position
-- is "dark". If the tile hasn't been generated
-- yet it is also considered dark
hasDarkNghbr :: TileMap -> Tile -> Bool
hasDarkNghbr ts = (> 0) . numDarkNghbrs ts

-- the number of dark neighbors a tile has
-- TODO: use for scaling darken value with
-- Int -> (a -> a) -> (a -> a) aka (nTimes)
numDarkNghbrs :: TileMap -> Tile -> Int
numDarkNghbrs ts t =
  let pr (Just t') = isDark t'
      pr Nothing   = True
      r6 = replicate 6
      -- how to avoid write this to avoid replicate 6 twice?
  in  length . Prelude.filter pr $ zipWith3 nghbr [N .. NW] (r6 $ pos t) (r6 ts)
