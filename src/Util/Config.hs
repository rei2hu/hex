module Util.Config where
-- random values used everywhere

startSeed :: Int
startSeed = 0

tileDarkThreshold, tileDarkenRate, tileLightenRate, tileBleedThreshold :: Float
tileDarkThreshold = 0.5 -- tiles with k values higher than this = dark
tileDarkenRate = 0.1 -- rate at which a tile darkens
tileLightenRate = 0.5 -- rate at which a tile lightens
tileBleedThreshold = 0.9 -- tiles with k values higher than this wont bleed

playerBleedRate :: Float
playerBleedRate = 0.1 -- rate at which the player bleeds

overlayBarHeight, overlayBarWidth, overlayBarPadding :: Float
overlayBarWidth = 20
overlayBarPadding = 5
--
overlayBarHeight = overlayBarWidth * 5

numberWidth, numberHeight, numberPadding :: Float
numberWidth = 7
numberPadding = 3
--
numberHeight = numberWidth * 1.8

hexagonSize, hexagonHeight, hexagonWidth, hexagonPadding, hexagonSelectorSize
  :: Float
hexagonSize = 30
--
hexagonHeight = sqrt 3 * hexagonSize
hexagonWidth = 2 * hexagonSize
hexagonPadding = 1 + 2 / hexagonSize
hexagonSelectorSize = hexagonSize + 1
