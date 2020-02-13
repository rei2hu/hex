module Graphics.Numbers where

import Graphics.Gloss.Data.Picture
import Util.Positioning

-- consider a digital clock display an 8
numWidth :: Float
numWidth = 7
numHeight = numWidth * 1.8
padding :: Float
padding = 3

-- draw a number centered at a position
-- warning on auto-formatting this file
drawNumber :: OffsetCoords -> Int -> Path
drawNumber (x, y) n = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomLeft = (xf * (numWidth + padding) - half, yf * (numHeight + padding) - halfH)
                          bottomRight = (xf * (numWidth + padding) + half, yf * (numHeight + padding) - halfH)
                          topRight = (xf * (numWidth + padding) + half, yf * (numHeight + padding) + halfH)
                      in case n of
                          0 -> path [(id, (+) numHeight), ((+) numWidth, id), (id, subtract numHeight), (subtract numWidth, id)] bottomLeft
                          1 -> path [(id, (+) numHeight)] bottomRight
                          2 -> path [(subtract numWidth, id), (id, (+) halfH), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] bottomRight
                          3 -> path [((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] bottomLeft
                          4 -> path [(id, (+) numHeight), (id, (-) halfH), (subtract numWidth, id), (id, (+) halfH)] bottomRight
                          5 -> path [((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id), (id, (+) halfH), ((+) numWidth, id)] bottomLeft
                          6 -> path [(subtract numWidth, id), (id, subtract numHeight), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] topRight
                          7 -> path [(id, (+) numHeight), (subtract numWidth, id)] bottomRight
                          8 -> path [(id, (+) numHeight), (subtract numWidth, id), (id, subtract numHeight), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] bottomRight
                          9 -> path [((+) numWidth, id), (id, (+) numHeight), (subtract numWidth, id), (id, subtract halfH), ((+) numWidth, id)] bottomLeft

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