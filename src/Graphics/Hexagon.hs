module Graphics.Hexagon where

import Graphics.Gloss.Data.Picture (line, Path)
import Util.Positioning (OffsetCoords)

-- some random constants
shapeSize :: Float
shapeSize = 30
shapeHeight = sqrt 3 * shapeSize
shapeWidth = 2 * shapeSize
padding = 1 + 2 / shapeSize

selectorSize :: Float
selectorSize = 32

-- returns a path which draws a hexagon centered at given coordinates
-- when traced
hexagonAt :: OffsetCoords -> Path
hexagonAt (x, y) = map (\i -> 
    let cm = pi / 180 * 60 * i
        yf = (fromIntegral y) + if (odd x) then 0.5 else 0
        xf = fromIntegral x
    in (padding * 0.75 * shapeWidth * xf + shapeSize * cos(cm), padding * shapeHeight * yf + shapeSize * sin(cm))) [0..6]


-- returns a path which draws a hexagon centered at given coordinates
-- when traced (difference is no padding)
selectorAt :: OffsetCoords -> Path
selectorAt (x, y) = map (\i -> 
    let cm = pi / 180 * 60 * i
        yf = (fromIntegral y) + if (odd x) then 0.5 else 0
        xf = fromIntegral x
    in (padding * 0.75 * shapeWidth * xf + selectorSize * cos(cm), padding * shapeHeight * yf + selectorSize * sin(cm))) [0..6]