module State.Game where
    
import State.Player
import Util.Positioning (cubeToOff, CubicCoords, OffsetCoords)
import Graphics.Gloss.Data.Color (Color)
import qualified Data.Map.Strict as Map
import qualified State.Tile as Tile

data Game = Game Player Tile.TileMap

newGame :: Game
newGame = Game (Player (0, 0)) $ Tile.makeTileAt (0, 0) Map.empty

getTiles :: Game -> [(OffsetCoords, Color)]
getTiles (Game _ t)= Map.foldr (\a b -> ((,) <$> Tile.positionOf <*> Tile.colorOf $ a):b) [] t

getPlayer :: Game -> OffsetCoords
getPlayer (Game p _) = positionOf p

movePlayer :: Char -> Game -> Game
movePlayer 'w' (Game p t) = Game (moveY 1 p) t
movePlayer 's' (Game p t) = Game (moveY (-1) p) t
movePlayer 'a' (Game p t) = Game (moveX (-1) p) t
movePlayer 'd' (Game p t) = Game (moveX 1 p) t
movePlayer _ g = g