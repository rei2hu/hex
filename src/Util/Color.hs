module Util.Color where

import Graphics.Gloss.Data.Color (makeColor, rgbaOfColor, Color)

-- 4 tuple of numbers between 0 and 1 that represent the color levels
-- of cmyk respectively
type Cmyk = (Float, Float, Float, Float)

cmykToColor :: Cmyk -> Color
cmykToColor c = let (r, g, b, a) = cmykToRgba c
                in makeColor r g b a

cmykToRgba :: Cmyk -> (Float, Float, Float, Float)
cmykToRgba (c, m, y, k) = let nk = (1 - k)
                         in ((1 - c) * nk, (1 - m) * nk, (1 - y) * nk, 1)

rgbaToCmyk :: (Float, Float, Float, Float) -> Cmyk
rgbaToCmyk (r, g, b, _) = (1 - r, 1 - g, 1 - b, 0)

colorToCmyk :: Color -> Cmyk
colorToCmyk = rgbaToCmyk . rgbaOfColor