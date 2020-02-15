module Util.Config where
-- random values used everywhere

startSeed :: Int
startSeed = 0

tileDarkThreshold, tileDarkenRate, tileLightThreshold, tileLightenRate, tileBleedThreshold :: Float
tileDarkThreshold = 0.5 -- tiles with k values higher than this are dark
tileLightThreshold = 0.1 -- tiles with k values less than this are light
tileLightenRate = 0.05 -- rate at which a tile lightens
tileBleedThreshold = 0.9 -- tiles with k values higher than this wont bleed
--
tileDarkenRate = tileLightenRate / 4 -- rate at which a tile darkens

playerBleedRate, playerBarfThreshold :: Float
playerBleedRate = 0.1 -- rate at which the player bleeds
playerBarfThreshold = 0.5 -- amount of cmy player needs to barf

overlayBarHeight, overlayBarWidth, overlayBarPadding :: Float
overlayPosition :: (Int, Int)
overlayPosition = (10, 0)
overlayBarWidth = 20 -- width of the cmy bars on the overlay
--
overlayBarPadding = overlayBarWidth / 4
overlayBarHeight = overlayBarWidth * 10

numberWidth, numberHeight, numberPadding :: Float
numberWidth = 7 -- width of drawn numbers
numberPadding = 3 -- padding between drawn numbers
--
numberHeight = numberWidth * 1.8

hexagonSize, hexagonHeight, hexagonWidth, hexagonPadding, hexagonSelectorSize
  :: Float
hexagonSize = 30 -- size of the hexagon
--
hexagonHeight = sqrt 3 * hexagonSize
hexagonWidth = 2 * hexagonSize
hexagonPadding = 1 + 2 / hexagonSize
hexagonSelectorSize = hexagonSize + 1
