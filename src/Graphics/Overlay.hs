module Graphics.Overlay where

import           State.Game                    as G
import           Graphics.Gloss.Data.Picture
import           Graphics.Gloss.Data.Color
import           Util.Positioning
import           State.Player                  as P
import           Graphics.Numbers              as N
import           Util.Config
import           State.Tile                    as T

data BarType = Cyan | Magenta | Yellow | Key deriving Enum

-- overlay x, y should 
overlay :: Game -> Picture
overlay g =
  let (c, m, y, _) = P.colors . G.player $ g
      (px, py)     = overlayPosition
  in  pictures
        $  zipWith3 bar
                    [Cyan .. Yellow]
                    [ (px + x, py) | x <- [0 .. 2] ]
                    [c, m, y]
        ++ [thresholdBar overlayPosition, tileRate overlayPosition g]


-- shows the coordinates of the player on the screen
coords :: Game -> Picture
coords g =
  let (x, y) = P.pos . G.player $ g
      fpaths z =
          map (\(i, c) -> (line . N.drawNumber (i, z)) c) . zip [1 ..] . show
  in  pictures $ concat $ zipWith fpaths [0, 1] [y, x]

tileRate :: OffsetCoords -> Game -> Picture
tileRate ps Game { G.player = pl, tiles = ts } =
  let t = T.getTileAt (P.pos pl) ts in color green $ N.drawNumbers ps $ T.rate ts t

-- draws a line that shows the barf threshold
thresholdBar :: OffsetCoords -> Picture
thresholdBar (x, y) =
  let fx =
          fromIntegral x
            * (overlayBarWidth + overlayBarPadding)
            - overlayBarWidth
            / 2
      fy = fromIntegral y * (overlayBarHeight + overlayBarPadding)
  in  color (greyN 0.5) $ line
        [ (fx, fy)
        , ( fx + (overlayBarPadding + overlayBarWidth) * 3 - overlayBarPadding
          , fy
          )
        ]

-- draws a bar on the screen with a certain height
-- and color
bar :: BarType -> OffsetCoords -> Float -> Picture
bar Cyan    p pct = color cyan $ _bar p pct
bar Magenta p pct = color magenta $ _bar p pct
bar Yellow  p pct = color yellow $ _bar p pct
bar Key     p pct = color black $ _bar p pct

_bar :: OffsetCoords -> Float -> Picture
_bar (x, y) pct =
  let
    fx = fromIntegral x * (overlayBarWidth + overlayBarPadding) :: Float
    fy = fromIntegral y * (overlayBarHeight + overlayBarPadding) :: Float
    bl@(blx, bly) = (fx - overlayBarWidth / 2, fy - overlayBarHeight / 2)
    tr@(trx, try) =
      (fx + overlayBarWidth / 2, fy + (pct - 0.5) * overlayBarHeight)
  in
    polygon [bl, (blx, try), tr, (trx, bly), bl]
