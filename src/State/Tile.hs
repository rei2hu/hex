module State.Tile where

import Graphics.Gloss.Data.Color
import Util.Positioning
import Util.Color
import qualified Data.Map.Strict as Map

type TileMap = Map.Map OffsetCoords Tile
data Tile = Tile OffsetCoords Cmyk deriving Show

makeTileAt :: OffsetCoords -> TileMap -> TileMap
makeTileAt c@(x, y) t = Map.insert c (Tile c (0, 0, 0, 0)) t

nghbr :: Int -> TileMap -> OffsetCoords -> Maybe Tile
nghbr 1 t (x, y) = Map.lookup (x, y - 1) t
nghbr 2 t (x, y) = Map.lookup (x + 1, y) t
nghbr 3 t (x, y) = Map.lookup (x + 1, y - 1) t
nghbr 4 t (x, y) = Map.lookup (x + 1, y - 1) t
nghbr 5 t (x, y) = Map.lookup (x, y + 1) t
nghbr 6 t (x, y) = Map.lookup (x - 1, y + 1) t
nghbr _ _ _ = error "invalid neighbor"

colorTo :: Color -> Tile -> Tile
colorTo c (Tile a _) = Tile a (colorToCmyk c)

colorOf :: Tile -> Color
colorOf (Tile _ c) = cmykToColor c

positionOf :: Tile -> OffsetCoords
positionOf (Tile a _) = a
