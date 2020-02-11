module Main where

import Graphics.Hexagon
import State.Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

window :: Display
window = InWindow "Hex" (800, 600) (10, 10)

background :: Color
background = black

drawing :: Game -> IO Picture
drawing g = do
    tiles <- return $ map (\(co, c) -> color c $ polygon $ hexagonAt co) $ getTiles g
    player <- return $ color red $ line $ selectorAt $ getPlayer g
    return $ pictures (player : tiles)

handler :: Event -> Game -> IO Game
handler (EventKey (SpecialKey KeyEsc) _ _ _) g = do
    error "exit"
    return g
handler (EventKey (Char c) _ _ _) g = do
    return $ movePlayer c g
handler _ g = do
    return g

calculate :: Float -> Game -> IO Game
calculate ms g = do
    return g

main :: IO ()
-- display window background drawing
-- Display -> Color -> Int -> world -> (world -> IO Picture) -> (Event -> world -> IO world) -> (Float -> world -> IO world) -> IO ()
main = playIO window black 1 newGame drawing handler calculate
-- main = display window background drawing