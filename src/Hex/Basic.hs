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
import Debug.Trace


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

shiftR :: Hex -> Hex
shiftR (V3 x y z) = V3 z x y

shiftL :: Hex -> Hex
shiftL = shiftR . shiftR

-- |Rotate a hex 60 degrees.
r60 :: Hex -> Hex -> Int -> Hex
r60 pivot v k
  | k == 0 = v
  | k == 1 = pivot + negate (V3 z x y)
  | k == 2 = pivot + V3 y z x
  | k == 3 = pivot + negate (V3 x y z)
  | k == 4 = pivot + V3 z x y
  | k == 5 = pivot + negate (V3 y z x)
  | otherwise = r60 pivot v (k `mod` 6)
    where V3 x y z = v - pivot

-- The logic behind the implementation below is that you can rotate a hex by
-- 120 degrees by taking the x coordinate and moving it to the back. You can
-- also mirror a hex by multiplying it by -1, which gives us the space in
-- between our 120 degree turns. A rotation of 3 is a special case of of
-- rotating clockwise 3 times and multiplying by -1, where 3 * 120 == no
-- rotation at all.
--r60 pivot v@(V3 x y z) 1 = negate $ pivot + (shiftL (v - pivot))
--r60 pivot v 2 = pivot + shiftR (v - pivot)
--r60 pivot v 3 = negate v
--r60 pivot v 4 = v
--r60 pivot v 5 = negate $ shiftR (v - pivot)
