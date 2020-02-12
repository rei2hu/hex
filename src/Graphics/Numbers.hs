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
                          bottomLeft@(blx, bly) = (xf * (numWidth + padding) - half, yf * (numHeight + padding) - halfH)
                      in [bottomLeft, (blx, bly + numHeight), (blx + numWidth, bly + numHeight), (blx + numWidth, bly), bottomLeft]
drawNumber (x, y) 1 = let half = numWidth / 2
                          halfH = numHeight / 2 -- numWidth
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomRight@(brx, bry) = (xf * (numWidth + padding) + half, yf * (numHeight + padding) - halfH)
                      in [bottomRight, (brx, bry + numHeight)]
drawNumber (x, y) 2 = let half = numWidth / 2
                          halfH = numHeight / 2 -- numWidth
                          xf = fromIntegral x
                          yf = fromIntegral y
                          bottomRight@(brx, bry) = (xf * (numWidth + padding) + half, yf * (numHeight + padding) - halfH)
                      in [bottomRight, (brx - numWidth, bry), (brx - numWidth, bry + halfH), (brx, bry + halfH), (brx, bry + numHeight), (brx - numWidth, bry + numHeight)]
drawNumber (x, y) 3 = [] 
drawNumber (x, y) 4 = []
drawNumber (x, y) 5 = []
drawNumber (x, y) 6 = []
drawNumber (x, y) 7 = []
drawNumber (x, y) 8 = []
drawNumber (x, y) 9 = []