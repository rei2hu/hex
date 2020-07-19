# hex

This is a simple game made as a learning project for Haskell. It's not very fleshed out, but
maybe passable if it was an early era mobile game. You can find a reflection of the project
[here](https://reimu.ws/posts/11)

#### About
It's an endless type game where the goal is to attempt to maintain as many white tiles as
possible.

#### Gameplay/Mechanics
1. The player (you) and tiles each have values for cmy(k)
   - `k` is only applicable for tiles
2. A revealed tile has a random amount of cmy(k)
3. The player sucks up cmy(k) from tiles it stands on if you are not full
4. The player can instantly wipe the cmy(k) values of a tile if it has enough cmy by dumping colors
   - This turns the tile white
5. Tiles lose `k` (turn whiter) if it is drained of cmy. Tiles gain `k` based on the number of
   surrounding dark tiles.
6. Fully dark tiles (`k` = 0) have their cmy values decay slowly. Fully white tiles (`k` = 1) slowly
   grow cmy

A ton of values are configurable in the `Config.hs` file

#### Controls
Movement: wasd

Dump Colors: p
