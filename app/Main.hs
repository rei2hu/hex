module Main where

import RenderHelper (hexagon, cmyk_to_color)
import Graphics.Gloss

window :: Display
window = InWindow "Hex" (800, 600) (10, 10)

background :: Color
background = black

drawing :: Picture
drawing = pictures [
    color (cmyk_to_color (1, 0, 0, 0)) (hexagon (0, 0)),
    color (cmyk_to_color (0, 1, 0, 0)) (hexagon (1, 0)),
    color (cmyk_to_color (0, 0, 1, 0)) (hexagon (1, 1))
    ]

main :: IO ()
main = display window background drawing