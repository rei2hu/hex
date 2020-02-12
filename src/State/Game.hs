module State.Game where
    
import qualified State.Player as Player
import Util.Positioning (cubeToOff, CubicCoords, OffsetCoords)
import Graphics.Gloss.Data.Color
import qualified Data.Map.Strict as Map
import qualified State.Tile as Tile
import Util.Color
import System.Random

data Game = Game { player :: Player.Player, tiles :: Tile.TileMap, rands :: [Float] }

game :: Player.Player -> Tile.TileMap -> Game
game pl ts = Game { player = pl, tiles = ts, rands = randoms $ mkStdGen 0 }

newGame :: Game
newGame = game (Player.player (0, 0)) $ Map.singleton (0, 0) $ Tile.tile (0, 0) $ colorToCmyk white

setPlayer :: Game -> Player.Player -> Game
setPlayer (Game { tiles = ts, rands = rs }) pl = Game { player = pl, tiles = ts, rands = rs }

setTiles :: Game -> Tile.TileMap -> Game
setTiles (Game { player = pl, rands = rs }) ts = Game { player = pl, tiles = ts, rands = rs }

setRands :: Game -> [Float] -> Game
setRands (Game { player = pl, tiles = ts }) rs = Game { player = pl, tiles = ts, rands = rs }

setSeed :: Game -> Int -> Game
setSeed (Game { player = pl, tiles = ts }) s = Game { player = pl, tiles = ts, rands = randoms $ mkStdGen s }

getRands :: Game -> Int -> (Game, [Float])
getRands g n = let rs = rands g
              in (setRands g (drop n rs), take n rs)

getTiles :: Game -> [(OffsetCoords, Color)]
getTiles g = Map.foldr (\a b -> ((,) <$> Tile.pos <*> cmykToColor . Tile.colors $ a):b) [] $ tiles g

getPlayer :: Game -> OffsetCoords
getPlayer g = Player.pos $ player g

movePlayer :: Game -> Char -> Game
movePlayer g 'w' = setPlayer g (Player.moveY (player g) 1)
movePlayer g 's' = setPlayer g (Player.moveY (player g) (-1))
movePlayer g 'a' = setPlayer g (Player.moveX (player g) (-1))
movePlayer g 'd' = setPlayer g (Player.moveX (player g) 1)
movePlayer g _ = g

revealTile :: Game -> OffsetCoords -> Game
revealTile g p = let (g', fs) = getRands g 4
                 in setTiles g' $ Tile.tileAt (tiles g') p fs

advance :: Game -> Float -> Game
advance g steps = let ts = tiles g
                  in setTiles g $ Map.map (\t -> if Tile.hasDarkNghbr ts t then Tile.darken t steps else t) ts
