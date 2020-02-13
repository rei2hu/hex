module Graphics.Hexagon where

import           Graphics.Gloss.Data.Picture
import           Util.Positioning

-- some random constants
shapeSize, shapeHeight, shapeWidth, padding, selectorSize :: Float
shapeSize = 30
shapeHeight = sqrt 3 * shapeSize
shapeWidth = 2 * shapeSize
padding = 1 + 2 / shapeSize
selectorSize = 31

-- returns a path which draws a hexagon centered at given coordinates
-- when traced
hexagonAt :: OffsetCoords -> Path
hexagonAt = hexagonPath shapeSize

-- returns a path which draws a hexagon centered at given coordinates
-- when traced (difference is no padding)
selectorAt :: OffsetCoords -> Path
selectorAt = hexagonPath selectorSize

hexagonPath :: Float -> OffsetCoords -> Path
hexagonPath sz (x, y) = map
  (\i ->
    let cm = pi / 180 * 60 * i
        yf = fromIntegral y + if odd x then 0.5 else 0
        xf = fromIntegral x
    in  ( padding * 0.75 * shapeWidth * xf + sz * cos cm
        , padding * shapeHeight * yf + sz * sin cm
        )
  )
  [0 .. 6]
