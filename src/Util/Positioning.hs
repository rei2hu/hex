module Util.Positioning where

type CubicCoords = (Int, Int, Int)
type OffsetCoords = (Int, Int)

-- converts cubic coordinates to offset coords
cubeToOff :: CubicCoords -> OffsetCoords
cubeToOff (p, q, r) = (p + floor ((fromIntegral r) / 2), r)

-- converts offset coordinates to cubic coords
offToCube :: OffsetCoords -> CubicCoords
offToCube (x, y) = (x - floor ((fromIntegral y) / 2), -x - y, y)
