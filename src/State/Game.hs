module State.Game where

import qualified State.Player                  as P
import           Util.Positioning
import           Graphics.Gloss.Data.Color
import qualified Data.Map.Strict               as M
import qualified State.Tile                    as T
import           Util.Color
import           System.Random

data Game = Game { player :: P.Player, tiles :: T.TileMap, rands :: [Float] }

-- returns a game with default state
newGame :: Game
newGame = Game { player = P.player
               , tiles  = M.singleton (0, 0) $ T.Tile (0, 0) $ colorToCmyk white
               , rands  = randoms $ mkStdGen 0
               }

-- sets the player of the game
setPlayer :: P.Player -> Game -> Game
setPlayer pl Game { tiles = ts, rands = rs } =
  Game { player = pl, tiles = ts, rands = rs }

-- sets the tiles of the game
setTiles :: T.TileMap -> Game -> Game
setTiles ts Game { player = pl, rands = rs } =
  Game { player = pl, tiles = ts, rands = rs }

-- sets the seed of the game
-- actually it refreshes the internal float list
setSeed :: Int -> Game -> Game
setSeed s Game { player = pl, tiles = ts } =
  Game { player = pl, tiles = ts, rands = randoms $ mkStdGen s }

-- gets several random floats from the games'
-- internal list
getRands :: Game -> Int -> (Game, [Float])
getRands g@Game { player = pl, tiles = ts } n =
  let rs = rands g
  in  (Game { player = pl, tiles = ts, rands = drop n rs }, take n rs)

-- returns a list of (positionOfTile, colorOfTile) tuples
getTiles :: Game -> [(OffsetCoords, Color)]
getTiles g =
  M.foldr (\a b -> ((,) <$> T.pos <*> cmykToColor . T.colors $ a) : b) []
    $ tiles g

-- moves a player in a direction
movePlayer :: Char -> Game -> Game
movePlayer 'w' g = setPlayer (P.moveY 1 (player g)) g
movePlayer 's' g = setPlayer (P.moveY (-1) (player g)) g
movePlayer 'a' g = setPlayer (P.moveX (-1) (player g)) g
movePlayer 'd' g = setPlayer (P.moveX 1 (player g)) g
movePlayer _   g = g

-- adds a tile to the game at the position
-- or does nothing if one already exists
revealTile :: OffsetCoords -> Game -> Game
revealTile p g =
  let (g', fs) = getRands g 4 in setTiles (T.mmakeTileAt fs p (tiles g')) g'

revealPlayerTile :: Game -> Game
revealPlayerTile = revealTile <$> P.pos . player <*> id

-- advances the game
advance :: Float -> Game -> Game
advance steps g@Game { tiles = ts, player = pl } = setTiles
  (M.map
    (\t -> if T.pos t == P.pos pl
      then T.lighten steps t
      else if T.hasDarkNghbr ts t then T.darken steps t else t
    )
    ts
  )
  g
