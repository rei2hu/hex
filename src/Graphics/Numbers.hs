module Graphics.Numbers where

import Graphics.Gloss.Data.Picture (line, Path)
import Util.Positioning (OffsetCoords)

-- consider a digital clock display an 8
numWidth :: Float
numWidth = 7
numHeight = numWidth * 1.8
padding :: Float
padding = 3

-- draw a number centered at a position
drawNumber :: OffsetCoords -> Int -> Path
drawNumber (x, y) 0 = let half = numWidth / 2
                          halfH = numHeight / 2 -- numWidth
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomLeft = (xf * (numWidth + padding) - half, yf * (numHeight + padding) - halfH)
                      in path [(id, (+) numHeight), ((+) numWidth, id), (id, subtract numHeight), (subtract numWidth, id)] bottomLeft
drawNumber (x, y) 1 = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomRight = (xf * (numWidth + padding) + half, yf * (numHeight + padding) - halfH)
                      in path [(id, (+) numHeight)] bottomRight
drawNumber (x, y) 2 = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomRight = (xf * (numWidth + padding) + half, yf * (numHeight + padding) - halfH)
                      in path [(subtract numWidth, id), (id, (+) halfH), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] bottomRight
drawNumber (x, y) 3 = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomLeft = (xf * (numWidth + padding) - half, yf * (numHeight + padding) - halfH)
                      in path [((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] bottomLeft
drawNumber (x, y) 4 = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomRight = (xf * (numWidth + padding) + half, yf * (numHeight + padding) - halfH)
                      in path [(id, (+) numHeight), (id, (-) halfH), (subtract numWidth, id), (id, (+) halfH)] bottomRight
drawNumber (x, y) 5 = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomLeft = (xf * (numWidth + padding) - half, yf * (numHeight + padding) - halfH)
                      in path [((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id), (id, (+) halfH), ((+) numWidth, id)] bottomLeft
drawNumber (x, y) 6 = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          topRight = (xf * (numWidth + padding) + half, yf * (numHeight + padding) + halfH)
                      in path [(subtract numWidth, id), (id, subtract numHeight), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] topRight
drawNumber (x, y) 7 = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomRight = (xf * (numWidth + padding) + half, yf * (numHeight + padding) - halfH)
                      in path [(id, (+) numHeight), (subtract numWidth, id)] bottomRight
drawNumber (x, y) 8 = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomRight = (xf * (numWidth + padding) + half, yf * (numHeight + padding) - halfH)
                      in path [(id, (+) numHeight), (subtract numWidth, id), (id, subtract numHeight), ((+) numWidth, id), (id, (+) halfH), (subtract numWidth, id)] bottomRight
drawNumber (x, y) 9 = let half = numWidth / 2
                          halfH = numHeight / 2
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomLeft = (xf * (numWidth + padding) - half, yf * (numHeight + padding) - halfH)
                      in path [((+) numWidth, id), (id, (+) numHeight), (subtract numWidth, id), (id, subtract halfH), ((+) numWidth, id)] bottomLeft

-- takes an set of x y transformations and an initial
-- point to generate a path. Is this written well?
-- probably not
-- maybe [(a, b) -> (a, b)] -> (a, b) -> [(a, b)]?
path :: [((a -> a), (b -> b))] -> (a, b) -> [(a, b)]
path a b = b : (tm a b)

tm :: [((a -> a), (b -> b))] -> (a, b) -> [(a, b)]
tm ((fa, fb):xs) (a, b) = let intl' = (fa a, fb b)
                          in (intl' : tm xs intl')
tm [] x = [x]