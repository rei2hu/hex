module Graphics.Overlay where

import           State.Game                    as G
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Data.Color
import           Util.Positioning
import           State.Player
import           Graphics.Numbers              as N

data BarType = Cyan | Magenta | Yellow | Key deriving Enum

barHeight, barWidth, padding :: Float
barHeight = 100
barWidth = 20
padding = 5

-- overlay x, y should 
overlay :: Game -> Picture
overlay g =
  let (c, m, y, _) = colors $ G.player g
  in  pictures $ color green (coords g) : zipWith3
        bar
        [Cyan .. Yellow]
        [ (5 + x, 0) | x <- [1 .. 3] ]
        [c, m, y]


-- shows the coordinates of the player on the screen
coords :: Game -> Picture
coords g =
  let (x, y) = (pos . G.player) g
      fpaths z =
          map (\(i, c) -> (line . N.drawNumber (i, z)) c) . zip [1 ..] . show
  in  pictures $ concat $ zipWith fpaths [0, 1] [y, x]


-- draws a bar on the screen with a certain height
-- and color
bar :: BarType -> OffsetCoords -> Float -> Picture
bar Cyan    p pct = color cyan $ _bar p pct
bar Magenta p pct = color magenta $ _bar p pct
bar Yellow  p pct = color yellow $ _bar p pct
bar Key     p pct = color black $ _bar p pct

_bar :: OffsetCoords -> Float -> Picture
_bar (x, y) pct =
  let fx = fromIntegral x * (barWidth + Graphics.Overlay.padding) :: Float
      fy = fromIntegral y * (barHeight + Graphics.Overlay.padding) :: Float
      bl@(blx, bly) = (fx - barWidth / 2, fy - barHeight / 2)
      tr@(trx, try) = (fx + barWidth / 2, fy + (pct - 0.5) * barHeight)
  in  polygon [bl, (blx, try), tr, (trx, bly), bl]
