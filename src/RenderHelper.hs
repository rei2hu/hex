module RenderHelper where

import Graphics.Gloss.Data.Picture (line, Path, Point, Picture)
import Graphics.Gloss.Data.Color (makeColorI, Color)

type CMYK = (Float, Float, Float, Float)

shapeSize = 30
shapeWidth = sqrt 3 * shapeSize
shapeHeight = 2 * shapeSize

hexagon :: Point -> Picture
hexagon = line . hexagon_points

hexagon_points :: Point -> Path
hexagon_points (x, y) = map (\i -> 
    let cm = pi / 180 * 60 * i
    in (0.75 * shapeHeight * x + shapeSize * cos(cm), shapeWidth * y + shapeSize * sin(cm))) [0..6]

cmyk_to_color :: CMYK -> Color
cmyk_to_color c = let (r, g, b) = cmyk_to_rgb c
                in makeColorI r g b 255

cmyk_to_rgb :: CMYK -> (Int, Int, Int)
cmyk_to_rgb (c, m, y, k) =
    let nk = (1 - k) * 255
    in (floor $ (1 - c) * nk, floor $ (1 - m) * nk, floor $ (1 - y) * nk)