module State.Game where
    
import State.Tile (Tile(..), positionOf, colorOf)
import Util.Positioning (cubeToOff, CubicCoords, OffsetCoords)
import Graphics.Gloss.Data.Color (Color)

getBoard :: [(OffsetCoords, Color)]
getBoard = map ((,) <$> positionOf <*> colorOf) [
    Tile (0, 0) (1, 0, 0 ,0),
    Tile (0, 1) (0, 1, 0, 0),
    Tile (1, 0) (0, 0, 1, 0),
    Tile (1, 1) (0, 1, 0, 0),
    Tile (-1, 0) (1, 0, 0, 0),
    Tile (0, -1) (0, 1, 0, 0),
    Tile (-1, -1) (0, 0, 1, 0)
    ]
