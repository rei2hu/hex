module Main where

import           Graphics.Hexagon
import           State.Game                    as G
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           System.Exit
import qualified State.Player                  as P
import           Graphics.Overlay

window :: Display
window = FullScreen -- InWindow "Hex" (800, 600) (10, 10)


drawing :: Game -> IO Picture
drawing g =
  let
    positions    = getTiles g
    ts = map (\(co, c) -> (color c . polygon . hexagonAt) co) positions
    outlines = map (\(co, _) -> (color white . line . hexagonAt) co) positions
    plp@(px, py) = (P.pos . G.player) g
    pl           = (color red . line . selectorAt) plp
    o            = overlay g
  in
    -- todo: -32 is shape size basically
    return $ pictures
      [ translate (fromIntegral px * (-32)) (fromIntegral py * (-32))
        $ pictures (pl : ts ++ outlines)
      , o
      ]

handler :: Event -> Game -> IO Game
handler (EventKey (SpecialKey KeyEsc) _ _ _) = return $ die "exit"
handler (EventKey (Char c) Down _ _) = return . revealPlayerTile . movePlayer c
handler _ = return

calculate :: Float -> Game -> IO Game
calculate ms g = return $ advance ms g

main :: IO ()
-- Display -> Color -> Int -> world -> (world -> IO Picture) -> (Event -> world -> IO world) -> (Float -> world -> IO world) -> IO ()
main = playIO window black 60 newGame drawing handler calculate
