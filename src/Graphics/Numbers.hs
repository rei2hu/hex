module Graphics.Numbers where

import Graphics.Gloss.Data.Picture
import Util.Positioning

-- consider a digital clock display an 8
numWidth, numHeight, padding :: Float
numWidth = 7
numHeight = numWidth * 1.8
padding = 3

-- draw a number centered at a position
-- warning on auto-formatting this file
drawNumber :: OffsetCoords -> Char -> Path
drawNumber (x, y) n = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x * (numWidth + padding)
                          yf = fromIntegral y * (numHeight + padding)
                          bottomLeft = (xf - half, yf - halfH)
                          bottomRight = (xf + half, yf - halfH)
                          topRight = (xf + half, yf + halfH)
                      in case n of
                          '-' -> path [((+) numWidth, id)] (xf - half, yf)
                          '0' -> path [(id, (+) numHeight), ((+) numWidth, id), (id, subtract numHeight), (subtract numWidth, id)] bottomLeft
                          '1' -> path [(id, (+) numHeight)] bottomRight
                          '2' -> path [(subtract numWidth, id), (id, (+) halfH), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] bottomRight
                          '3' -> path [((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] bottomLeft
                          '4' -> path [(id, (+) numHeight), (id, subtract halfH), (subtract numWidth, id), (id, (+) halfH)] bottomRight
                          '5' -> path [((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id), (id, (+) halfH), ((+) numWidth, id)] bottomLeft
                          '6' -> path [(subtract numWidth, id), (id, subtract numHeight), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] topRight
                          '7' -> path [(id, (+) numHeight), (subtract numWidth, id)] bottomRight
                          '8' -> path [(id, (+) numHeight), (subtract numWidth, id), (id, subtract numHeight), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] bottomRight
                          '9' -> path [((+) numWidth, id), (id, (+) numHeight), (subtract numWidth, id), (id, subtract halfH), ((+) numWidth, id)] bottomLeft
                          nm -> error $ "invalid number to be drawn: " ++ show nm

-- takes an set of x y transformations and an initial
-- point to generate a path. Is this written well?
-- probably not
-- maybe [(a, b) -> (a, b)] -> (a, b) -> [(a, b)]?
path :: [(a -> a, b -> b)] -> (a, b) -> [(a, b)]
path a b = b : tm a b

tm :: [(a -> a, b -> b)] -> (a, b) -> [(a, b)]
tm ((fa, fb):xs) (a, b) = let intl' = (fa a, fb b)
                          in (intl' : tm xs intl')
tm [] x = [x]