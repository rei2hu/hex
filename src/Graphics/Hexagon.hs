module Graphics.Hexagon where

import           Graphics.Gloss.Data.Picture
import           Util.Positioning
import           Util.Config

-- returns a path which draws a hexagon centered at given coordinates
-- when traced
hexagonAt :: OffsetCoords -> Path
hexagonAt = hexagonPath hexagonSize

-- returns a path which draws a hexagon centered at given coordinates
-- when traced (difference is no padding)
selectorAt :: OffsetCoords -> Path
selectorAt = hexagonPath hexagonSelectorSize

hexagonPath :: Float -> OffsetCoords -> Path
hexagonPath sz (x, y) = map
  (\i ->
    let cm = pi / 180 * 60 * i
        yf = fromIntegral y + if odd x then 0.5 else 0
        xf = fromIntegral x
    in  ( hexagonPadding * 0.75 * hexagonWidth * xf + sz * cos cm
        , hexagonPadding * hexagonHeight * yf + sz * sin cm
        )
  )
  [0 .. 6]
