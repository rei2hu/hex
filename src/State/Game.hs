module State.Game where
    
import qualified State.Player as Player
import Util.Positioning (cubeToOff, CubicCoords, OffsetCoords)
import Graphics.Gloss.Data.Color (Color)
import qualified Data.Map.Strict as Map
import qualified State.Tile as Tile
import Util.Color

data Game = Game { player :: Player.Player, tiles :: Tile.TileMap }

game :: Player.Player -> Tile.TileMap -> Game
game pl ts = Game { player = pl, tiles = ts }

newGame :: Game
newGame = game (Player.player (0, 0)) $ Tile.tileAt Map.empty (0, 0)

getTiles :: Game -> [(OffsetCoords, Color)]
getTiles (Game _ ts)= Map.foldr (\a b -> ((,) <$> Tile.pos <*> cmykToColor . Tile.colors $ a):b) [] ts

getPlayer :: Game -> OffsetCoords
getPlayer (Game { player = pl }) = Player.pos pl

movePlayer :: Game -> Char -> Game
movePlayer (Game { player = pl, tiles = ts }) 'w' = game (Player.moveY pl 1) ts
movePlayer (Game { player = pl, tiles = ts }) 's' = game (Player.moveY pl (-1)) ts
movePlayer (Game { player = pl, tiles = ts }) 'a' = game (Player.moveX pl (-1)) ts
movePlayer (Game { player = pl, tiles = ts }) 'd' = game (Player.moveX pl 1) ts
movePlayer g _ = g

revealTile :: Game -> OffsetCoords -> Game
revealTile (Game { player = pl, tiles = ts }) p= game pl $ Tile.tileAt ts p

advance :: Game -> Float -> Game
advance (Game { player = pl, tiles = ts }) steps = game pl $ Map.map (\t -> if Tile.hasDarkNghbr ts t then Tile.darken t steps else t) ts