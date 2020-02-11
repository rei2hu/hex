module Main where

import Graphics.Hexagon
import State.Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit

window :: Display
window = InWindow "Hex" (800, 600) (10, 10)

drawing :: Game -> IO Picture
drawing g = let tiles = map (\(co, c) -> color c $ polygon $ hexagonAt co) $ getTiles g
                player = color red $ line $ selectorAt $ getPlayer g
            in return $ pictures (player : tiles)

handler :: Event -> Game -> IO Game
handler (EventKey (SpecialKey KeyEsc) _ _ _) g = do
    die "exit"
    return g
handler (EventKey (Char c) Down _ _) g = return $ revealTile <$> getPlayer <*> id $ movePlayer c g
handler _ g = return g

calculate :: Float -> Game -> IO Game
calculate ms = return . advance ms

main :: IO ()
-- display window background drawing
-- Display -> Color -> Int -> world -> (world -> IO Picture) -> (Event -> world -> IO world) -> (Float -> world -> IO world) -> IO ()
main = playIO window black 1 newGame drawing handler calculate
-- main = display window background drawing