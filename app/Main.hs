module Main where

import Graphics.Hexagon (hexagonAt, selectorAt)
import State.Game (getBoard)
import Graphics.Gloss

window :: Display
window = InWindow "Hex" (800, 600) (10, 10)

background :: Color
background = black

drawing :: Picture
drawing = pictures $ (color red $ line $ selectorAt (0, 0)) : map (\(co, c) -> color c $ polygon $ hexagonAt co) getBoard

main :: IO ()
main = display window background drawing