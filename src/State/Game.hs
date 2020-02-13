module State.Game where

import qualified State.Player                  as P
import           Util.Positioning
import           Graphics.Gloss.Data.Color
import qualified Data.Map.Strict               as M
import qualified State.Tile                    as T
import           Util.Color
import           System.Random

data Game = Game { player :: P.Player, tiles :: T.TileMap, rands :: [Float] }

game :: P.Player -> T.TileMap -> Game
game pl ts = Game { player = pl, tiles = ts, rands = randoms $ mkStdGen 0 }

newGame :: Game
newGame =
  game P.player $ M.singleton (0, 0) $ T.tile (0, 0) $ colorToCmyk white

setPlayer :: Game -> P.Player -> Game
setPlayer Game { tiles = ts, rands = rs } pl =
  Game { player = pl, tiles = ts, rands = rs }

setTiles :: Game -> T.TileMap -> Game
setTiles Game { player = pl, rands = rs } ts =
  Game { player = pl, tiles = ts, rands = rs }

setRands :: Game -> [Float] -> Game
setRands Game { player = pl, tiles = ts } rs =
  Game { player = pl, tiles = ts, rands = rs }

setSeed :: Game -> Int -> Game
setSeed Game { player = pl, tiles = ts } s =
  Game { player = pl, tiles = ts, rands = randoms $ mkStdGen s }

getRands :: Game -> Int -> (Game, [Float])
getRands g n = let rs = rands g in (setRands g (drop n rs), take n rs)

getTiles :: Game -> [(OffsetCoords, Color)]
getTiles g =
  M.foldr (\a b -> ((,) <$> T.pos <*> cmykToColor . T.colors $ a) : b) []
    $ tiles g

getPlayer :: Game -> OffsetCoords
getPlayer g = P.pos $ player g

movePlayer :: Game -> Char -> Game
movePlayer g 'w' = setPlayer g (P.moveY (player g) 1)
movePlayer g 's' = setPlayer g (P.moveY (player g) (-1))
movePlayer g 'a' = setPlayer g (P.moveX (player g) (-1))
movePlayer g 'd' = setPlayer g (P.moveX (player g) 1)
movePlayer g _   = g

revealTile :: Game -> OffsetCoords -> Game
revealTile g p =
  let (g', fs) = getRands g 4 in setTiles g' $ T.tileAt (tiles g') p fs

advance :: Game -> Float -> Game
advance g steps =
  let ts = tiles g
  in  setTiles g $ M.map
        (\t -> if T.pos t == getPlayer g
          then T.lighten t steps
          else if T.hasDarkNghbr ts t then T.darken t steps else t
        )
        ts
