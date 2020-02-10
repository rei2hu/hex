module State.Tile where

import Graphics.Gloss.Data.Color (Color)
import Util.Positioning (CubicCoords)
import Util.Color (Cmyk, cmykToColor)

data Tile a = Tile a Cmyk deriving Show

colorOf :: Tile a -> Color
colorOf (Tile _ c) = cmykToColor c

positionOf :: Tile a -> a
positionOf (Tile a _) = a

instance Functor Tile where
    fmap f (Tile p c) = Tile (f p) c