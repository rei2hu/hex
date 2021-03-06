module Util.Color where

import           Graphics.Gloss.Data.Color

-- 4 tuple of numbers between 0 and 1 that represent the color levels
-- of cmyk respectively
type Cmyk = (Float, Float, Float, Float)

cmykToColor :: Cmyk -> Color
cmykToColor c = let (r, g, b, a) = cmykToRgba c in makeColor r g b a

cmykToRgba :: Cmyk -> (Float, Float, Float, Float)
cmykToRgba (c, m, y, k) =
  let nk = (1 - k) in ((1 - c) * nk, (1 - m) * nk, (1 - y) * nk, 1)

rgbaToCmyk :: (Float, Float, Float, Float) -> Cmyk
rgbaToCmyk (r, g, b, _) = (1 - r, 1 - g, 1 - b, 0)

colorToCmyk :: Color -> Cmyk
colorToCmyk = rgbaToCmyk . rgbaOfColor

addCmyk :: Cmyk -> Cmyk -> Cmyk
addCmyk (c, m, y, k) (c', m', y', k') = 
  let clamp = min 1 . max 0
  in (clamp (c + c'), clamp (m + m'), clamp (y + y'), clamp (k + k'))

subCmyk :: Cmyk -> Cmyk -> Cmyk
subCmyk z (c, m, y, k) = addCmyk (-c, -m, -y, -k) z
