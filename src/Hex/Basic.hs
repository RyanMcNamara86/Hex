module Hex.Basic
    ( Hex
    , origin
    , hexNum
    , perimeter
    , radius
    , neighbors
    , diagonals
    , dist
    , inRange
    , intersection
    , r60
    , module Linear.V3
    ) where

import Data.List
import Linear.V3


type Hex = V3 Int


-- |The center of the world.
origin :: Hex
origin = V3 0 0 0


-- |OEIS: A003215; Hex Numbers or Crystal Ball Sequence for Hexagonal Lattice
hexNum :: Int -> Int
hexNum n = 3 * n * (n + 1) + 1

perimeter :: Int -> Int
perimeter = (6*)


radius :: Int -> Int
radius 0 = 0
radius x = length $ takeWhile (x >=) areas
    where areas = hexNum <$> [0..]


-- |Distance in number of hexagonal tiles.
dist :: Hex -> Hex -> Int
dist a b = (sum . abs $ a - b) `div` 2


-- |List all adjacent hexes.
neighbors :: Hex -> [Hex]
neighbors (V3 x y z) =
    let mkhex [a, b, c] = V3 (a+x) (b+y) (c+z)
     in mkhex <$> (permutations [1, -1, 0])


-- |The hexes, besides the immediately adjacent hexes, directly
-- attached to a center hex by a single line semgment. These are
-- considered the equivalent of a diagonal move for a hexagonal grid.
diagonals :: Hex -> [Hex]
diagonals (V3 x y z) = [ V3 (x + 2) (y - 1) (z - 1)
                       , V3 (x + 1) (y + 1) (z - 2)
                       , V3 (x - 1) (y + 2) (z - 1)
                       , V3 (x - 2) (y + 1) (z + 1)
                       , V3 (x - 1) (y - 1) (z + 2)
                       , V3 (x + 1) (y - 2) (z + 1)
                       ]


-- |Hexagons which are within "range" of the reference tile.
inRange :: Hex -> Int -> [Hex]
inRange (V3 x y z) range =
    let mkRange ref = [ref-range .. ref+range]
     in [ V3 a b c | a <- mkRange x
                   , b <- mkRange x
                   , c <- mkRange x
                   , a + b + c == 0 ]


-- |The set of hexagons which are within "range" tiles of the both reference
-- tiles.
intersection :: Hex -> Hex -> Int -> [Hex]
intersection a b range =
    [hex | hex <- inRange a range, hex `elem` inRange b range]


-- |Rotate a hex 60 degrees.
r60 :: Hex -> Hex -> Int -> Hex
r60 pivot (V3 x y z) k =
    case k of 0 -> V3 x y z
              1 -> V3 (-z) (-x) (-y)
              2 -> V3 y z x
              3 -> V3 (-x) (-y) (-z)
              4 -> V3 z x y
              5 -> V3 (-y) (-z) (-x)
              otherwise -> r60 pivot (V3 x y z) (k `mod` 6)
