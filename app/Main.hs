module Main where

import Graphics.Hexagon
import Graphics.Numbers
import State.Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import qualified State.Player as Player

window :: Display
window = InWindow "Hex" (800, 600) (10, 10)

drawing :: Game -> IO Picture
drawing g = let positions = getTiles g
                tiles = map (\(co, c) -> (color c . polygon . hexagonAt) co) positions
                outlines = map (\(co, _) -> (color white . line . outlineAt) co) positions
                player = color red $ line $ selectorAt $ getPlayer g
                nums = map (\x -> color green $ line $ drawNumber (x, 0) x) [0..9]
            in return $ pictures (player : tiles ++ outlines ++ nums)

handler :: Event -> Game -> IO Game
handler (EventKey (SpecialKey KeyEsc) _ _ _) g = do
    die "exit"
    return g
handler (EventKey (Char c) Down _ _) g = return $ revealTile <$> id <*> (Player.pos . player) $ movePlayer g c
handler _ g = return g

calculate :: Float -> Game -> IO Game
calculate ms g = return $ advance g ms

main :: IO ()
-- Display -> Color -> Int -> world -> (world -> IO Picture) -> (Event -> world -> IO world) -> (Float -> world -> IO world) -> IO ()
main = playIO window black 60 newGame drawing handler calculate