module State.Tile where

import           Util.Positioning
import           Util.Color
import           Util.Config
import           Data.Map.Strict

type TileMap = Map OffsetCoords Tile
data Tile = Tile { pos :: OffsetCoords, colors :: Cmyk } deriving Show
data Neighbor = N | NE | SE | S | SW | NW deriving Enum

-- sets the color of a tile
setColor :: Cmyk -> Tile -> Tile
setColor c t = Tile { pos = pos t, colors = c }

-- determines if a tile is dark
isDark :: Tile -> Bool
isDark Tile { colors = (_, _, _, k) } = k > tileDarkThreshold

-- determines if a tile is light
isLight :: Tile -> Bool
isLight Tile { colors = (_, _, _, k) } = k < tileLightThreshold

-- advances a tile (either lightens or darknes it)
advance :: TileMap -> Float -> [Float] -> Tile -> Tile
advance ts s rs t@Tile { colors = (c, m, y, _) } =
  -- darken is scaled by number of dark neighbors
  let s' = s * fromIntegral (numDarkNghbrs ts t)
  in  case (c, m, y) of
                            -- if a tile is drained, then we both lighten and darken it
        (0, 0, 0) -> darken rs s' . lighten rs s $ t
        -- or else we just darken it
        _         -> darken rs s' t

-- darkens a tile by increasing its "key" value
-- if a tile is fully darkened, then it starts losing cmy
darken :: [Float] -> Float -> Tile -> Tile
darken rs s t =
  let [c', m', y'] = Prelude.map (* tileDarkDecayRate) rs
      (c, m, y, k) = colors t
      mx0          = max 0
  in  case k of
        _ | k < 1 -> setColor (c, m, y, min 1 (k + tileDarkenRate * s)) t
        _         -> setColor (mx0 (c - c'), mx0 (m - m'), mx0 (y - y'), k) t

-- lightens a tile by decreasing its "key" value
-- if a tile is fully lightened, it starts gaining cmy
lighten :: [Float] -> Float -> Tile -> Tile
lighten rs s t =
  let [c', m', y'] = Prelude.map (* tileLightGrowRate) rs
      (c, m, y, k) = colors t
      mn1          = min 1
  in  case k of
        _ | k > 0 -> setColor (c, m, y, max 0 (k - tileLightenRate * s)) t
        _         -> setColor (mn1 (c + c'), mn1 (m + m'), mn1 (y + y'), 0) t

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
numDarkNghbrs :: TileMap -> Tile -> Int
numDarkNghbrs ts t =
  let pr (Just t') = isDark t'
      pr Nothing   = True
      r6 = replicate 6
      -- how to avoid write this to avoid replicate 6 twice?
  in  length . Prelude.filter pr $ zipWith3 nghbr [N .. NW] (r6 $ pos t) (r6 ts)
