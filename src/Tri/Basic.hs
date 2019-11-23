module Tri.Basic
    ( Tri
    , origin
    , triNum
    , perimeter
    , dist
    , isOdd
    , isEven
    , neighbors
    , diagonals
    ) where

import Data.List
import Linear.V3


type Tri = V3 Int


-- |The center of the world.
origin :: Tri
origin = V3 0 0 0


-- |OEIS: A005448; area within n tiles from the center
triNum :: Int -> Int
triNum n = 3 * n * (n - 1) `div` 2 + 1


perimeter :: Int -> Int
perimeter = (3*)


-- |Distance in number of triangular tiles.
dist :: Tri -> Tri -> Int
dist a b = sum $ abs (a - b)


-- |An even-parity triangle is a triangle with the pointy-side up.
isEven :: Tri -> Bool
isEven v = sum v `mod` 2 == 0


-- |An odd-parity triangle is a triangle with the pointy-side down.
isOdd :: Tri -> Bool
isOdd = not . isEven


-- |List all adjacent tris.
neighbors :: Tri -> [Tri]
neighbors tri = (tri +/-) <$> [V3 1 0 0, V3 0 1 0, V3 0 0 1] 
    where (+/-) = if isEven tri then (+) else (-)


-- |The diagonal tris are those tris which are connect to a central tri by a
-- vertex, rather than an edge. It is analogous the the diagonals of a
-- rectangular grid.
diagonals :: Tri -> [Tri]
diagonals v 
  | isEven v = map (v+) [ V3 0 (-1) 1,   V3 1 (-1) 1   ,   V3 1 (-1) 0
                        , V3 1 0 (-1),   V3 1 1 (-1)   ,   V3 0 1 (-1)
                        , V3 (-1) 1 0,   V3 (-1) 1 1   ,   V3 (-1) 0 1 ]

  | isOdd v  = map (v+) [ V3 1 (-1) 0,   V3 1 (-1) (-1),   V3 1 0 (-1)
                        , V3 0 1 (-1),   V3 (-1) 1 (-1),   V3 (-1) 1 0
                        , V3 (-1) 0 1,   V3 (-1) (-1) 1,   V3 0 (-1) 1 ]


---- |Hexagons which are within "range" of the reference tile.
--inRange :: Hex -> Int -> [Hex]
--inRange (V3 x y z) range =
--    let mkRange ref = [ref-range .. ref+range]
--     in [ V3 a b c | a <- mkRange x
--                   , b <- mkRange x
--                   , c <- mkRange x
--                   , a + b + c == 0 ]
--
--
---- |The set of hexagons which are within "range" tiles of the both reference
---- tiles.
--intersection :: Hex -> Hex -> Int -> [Hex]
--intersection a b range =
--    [hex | hex <- inRange a range, hex `elem` inRange b range]
--
--
---- |Rotate a hex 60 degrees.
--r60 :: Hex -> Hex -> Int -> Hex
--r60 pivot (V3 x y z) k =
--    case k of 0 -> V3 x y z
--              1 -> V3 (-z) (-x) (-y)
--              2 -> V3 y z x
--              3 -> V3 (-x) (-y) (-z)
--              4 -> V3 z x y
--              5 -> V3 (-y) (-z) (-x)
--              otherwise -> r60 pivot (V3 x y z) (k `mod` 6)
