module State.Tile where

import Graphics.Gloss.Data.Color
import Util.Positioning
import Util.Color
import Data.Maybe
import qualified Data.Map.Strict as Map

type TileMap = Map.Map OffsetCoords Tile
data Tile = Tile OffsetCoords Cmyk deriving Show

makeTileAt :: OffsetCoords -> TileMap -> TileMap
makeTileAt c@(x, y) ts = Map.insert c (Tile c (1, 0, 0, 0.6)) ts

tileAt :: OffsetCoords -> TileMap -> TileMap
tileAt c ts = case Map.lookup c ts of
                Nothing -> makeTileAt c ts
                (Just t) -> ts

nghbr :: Int -> TileMap -> OffsetCoords -> Maybe Tile
nghbr 1 t (x, y) = Map.lookup (x, y - 1) t
nghbr 2 t (x, y) = Map.lookup (x + 1, y) t
nghbr 3 t (x, y) = Map.lookup (x + 1, y - 1) t
nghbr 4 t (x, y) = Map.lookup (x + 1, y - 1) t
nghbr 5 t (x, y) = Map.lookup (x, y + 1) t
nghbr 6 t (x, y) = Map.lookup (x - 1, y + 1) t
nghbr _ _ _ = error "invalid neighbor"

hasDarkNghbr :: Tile -> TileMap -> Bool
hasDarkNghbr (Tile (x, y) _) ts = let pred (Just (Tile _ (_, _, _, k))) = k > 0.5
                                      pred Nothing = True
                                  in any pred $ zipWith (uncurry nghbr) (zip [1..6] (replicate 6 ts)) (replicate 6 (x, y))

colorTo :: Color -> Tile -> Tile
colorTo c (Tile a _) = Tile a (colorToCmyk c)

colorOf :: Tile -> Color
colorOf (Tile _ c) = cmykToColor c

positionOf :: Tile -> OffsetCoords
positionOf (Tile a _) = a

darken :: Float -> Tile -> Tile
darken s (Tile p (c, m, y, k)) = Tile p (c, m, y, min 1 (k + 0.1 * s))