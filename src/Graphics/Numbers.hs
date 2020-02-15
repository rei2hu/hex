module Graphics.Numbers where

import           Graphics.Gloss.Data.Picture
import           Util.Positioning
import           Util.Config

-- draws a number at a position
drawNumbers :: OffsetCoords -> Int -> Picture
drawNumbers (x, y) = pictures . map (\(i, c) -> (line . drawNumber (x + i, y)) c) . zip [1 ..] . show

-- draw a number centered at a position
-- warning on auto-formatting this file
drawNumber :: OffsetCoords -> Char -> Path
drawNumber (x, y) n =
  let
    half        = numberWidth / 2
    halfH       = numberHeight / 2
    xf          = fromIntegral x * (numberWidth + numberPadding)
    yf          = fromIntegral y * (numberHeight + numberPadding)
    bottomLeft  = (xf - half, yf - halfH)
    bottomRight = (xf + half, yf - halfH)
    topRight    = (xf + half, yf + halfH)
  in
    case n of
      '-' -> path [((+) numberWidth, id)] (xf - half, yf)
      '0' -> path
        [ (id                  , (+) numberHeight)
        , ((+) numberWidth     , id)
        , (id                  , subtract numberHeight)
        , (subtract numberWidth, id)
        ]
        bottomLeft
      '1' -> path [(id, (+) numberHeight)] bottomRight
      '2' -> path
        [ (subtract numberWidth, id)
        , (id                  , (+) halfH)
        , ((+) numberWidth     , id)
        , (id                  , (+) halfH)
        , (subtract numberWidth, id)
        ]
        bottomRight
      '3' -> path
        [ ((+) numberWidth     , id)
        , (id                  , (+) halfH)
        , (subtract numberWidth, id)
        , ((+) numberWidth     , id)
        , (id                  , (+) halfH)
        , (subtract numberWidth, id)
        ]
        bottomLeft
      '4' -> path
        [ (id                  , (+) numberHeight)
        , (id                  , subtract halfH)
        , (subtract numberWidth, id)
        , (id                  , (+) halfH)
        ]
        bottomRight
      '5' -> path
        [ ((+) numberWidth     , id)
        , (id                  , (+) halfH)
        , (subtract numberWidth, id)
        , (id                  , (+) halfH)
        , ((+) numberWidth     , id)
        ]
        bottomLeft
      '6' -> path
        [ (subtract numberWidth, id)
        , (id                  , subtract numberHeight)
        , ((+) numberWidth     , id)
        , (id                  , (+) halfH)
        , (subtract numberWidth, id)
        ]
        topRight
      '7' ->
        path [(id, (+) numberHeight), (subtract numberWidth, id)] bottomRight
      '8' -> path
        [ (id                  , (+) numberHeight)
        , (subtract numberWidth, id)
        , (id                  , subtract numberHeight)
        , ((+) numberWidth     , id)
        , (id                  , (+) halfH)
        , (subtract numberWidth, id)
        ]
        bottomRight
      '9' -> path
        [ ((+) numberWidth     , id)
        , (id                  , (+) numberHeight)
        , (subtract numberWidth, id)
        , (id                  , subtract halfH)
        , ((+) numberWidth     , id)
        ]
        bottomLeft
      nm -> error $ "invalid number to be drawn: " ++ show nm

-- takes an set of x y transformations and an initial
-- point to generate a path. Is this written well?
-- probably not
-- maybe [(a, b) -> (a, b)] -> (a, b) -> [(a, b)]?
path :: [(a -> a, b -> b)] -> (a, b) -> [(a, b)]
path a b = b : tm a b

tm :: [(a -> a, b -> b)] -> (a, b) -> [(a, b)]
tm ((fa, fb) : xs) (a, b) = let intl' = (fa a, fb b) in (intl' : tm xs intl')
tm []              x      = [x]
