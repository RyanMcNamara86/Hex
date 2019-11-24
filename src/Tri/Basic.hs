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
    , inRange
    , bfs
    , r120
    ) where

import Data.List
import Linear.V3


type Tri = V3 Int


-- |The center of the world.
origin :: Tri
origin = V3 0 0 0


-- |OEIS: A005448; area within n tiles from the center
triNum :: Int -> Int
triNum 0 = 0
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


bfs :: Tri -> [Tri]
bfs center = center : _bfs [center] [center]

_bfs :: [Tri] -> [Tri] -> [Tri]
_bfs [] _ = []
_bfs queue visited = q ++ _bfs q v
  where q = nub $ filter (`notElem` visited) (concat $ neighbors <$> queue)
        v = queue ++ visited


-- |Hexagons which are within "range" of the reference tile.
inRange :: Tri -> Int -> [Tri]
inRange center range = take (triNum range) (bfs center)

-- |The set of hexagons which are within "range" tiles of the both reference
-- tiles.
intersection :: Tri -> Tri -> Int -> [Tri]
intersection a b range =
    [tri | tri <- inRange a range, tri `elem` inRange b range]


-- There's an interesting symmetry with these functions that is probably more
-- obvious to someone better at geometry and linear algebra than me.
mirrorX :: Tri -> Tri
mirrorX (V3 x y z) = V3 x z y

mirrorY :: Tri -> Tri
mirrorY (V3 x y z) = V3 z y x

mirrorZ :: Tri -> Tri
mirrorZ (V3 x y z) = V3 y x z

rotateL :: Tri -> Tri
rotateL (V3 x y z) = V3 y z x

rotateR :: Tri -> Tri
rotateR (V3 x y z) = V3 z x y


-- |Rotate a hex 120 degrees.
r120 :: Tri -> Tri -> Int -> Tri
r120 pivot v 0 = v
r120 pivot v 1 = pivot + rotateR (v - pivot)
r120 pivot v 2 = pivot + rotateL (v - pivot)
r120 pivot v k = r120 pivot v (k `mod` 3)
