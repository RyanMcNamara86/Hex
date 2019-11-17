module Hex where

import Data.List
import Control.Monad (guard)
import Data.Tree
import Debug.Trace

import Linear.V3
import Linear.V2

import Conversion

type Hex = V3 Int

-- These two below are used to test maps with blocked tiles.
--blocked = [ False, False, True , False,
--            False, True , False, False,
--            True , True , True , True ,
--            False, True , True , False,
--            False, True , True        ]

-- |The center of the world.
origin :: Hex
origin = V3 0 0 0


-- OEIS: A003215; Hex Numbers or Crystal Ball Sequence for Hexagonal Lattice
hexNum :: Int -> Int
hexNum n = 3 * n * (n + 1) + 1


perimeter :: Int -> Int
perimeter = (6*)


-- |The hex to the right of the reference hex on the left-right axis.
right :: Hex
right = V3 1 (-1) 0

-- |The hex to the left of the reference hex on the left-right axis.
left :: Hex
left = V3 (-1) 1 0

-- |The hex to the left and up of the reference hex on the NW-SE axis.
leftup :: Hex
leftup = V3 0 1 (-1)

-- |The hex to the right and down of the reference hex on the NW-SE axis.
rightdown :: Hex
rightdown = V3 0 (-1) 1

-- |The hex to the right and up of the reference hex on the SW-NE axis.
rightup :: Hex
rightup = V3 1 0 (-1)

-- |The hex to the left and down from the reference hex on the SW-NE axis.
leftdown :: Hex
leftdown = V3 (-1) 0 1


-- |Some helper functions for spiral.
radius :: Int -> Int
radius 0 = 0
radius x = length $ takeWhile (x >=) areas
    where areas = hexNum <$> [0..]

neighbors :: Hex -> [Hex]
neighbors (V3 x y z) =
    let mkhex [a, b, c] = V3 (a+x) (b+y) (c+z)
     in mkhex <$> (permutations [1, -1, 0])


diagonals :: Hex -> [Hex]
diagonals (V3 x y z) = [ V3 (x + 2) (y - 1) (z - 1)
                       , V3 (x + 1) (y + 1) (z - 2)
                       , V3 (x - 1) (y + 2) (z - 1)
                       , V3 (x - 2) (y + 1) (z + 1)
                       , V3 (x - 1) (y - 1) (z + 2)
                       , V3 (x + 1) (y - 2) (z + 1)
                       ]


fscan :: [a -> a] -> a -> [a]
fscan [] _ = []
fscan (f:fs) x = f x : fscan fs (f x)

-- |Tiles in the ring r tiles away from the center
ring :: Int    -- ^radius
     -> Hex    -- ^center / ring 0
     -> [Hex]
ring 0 center = [center]
ring r v = 
    let dirs = [left, leftdown, rightdown, right, rightup, leftup]
    -- Turn each caridinal direction into a function.
        getCorner dir = iterate (+dir) v !! r
    -- each corner is found by going in a cardinal direction radius times:
        corners = getCorner <$> dirs
    -- every consecutive pair of corners ("++ head corners" wraps around list):
        adjPairs = zip <*> tail $ (corners ++ [head corners])
     in concat $ init <$> uncurry getPath <$> adjPairs


spiralToRing :: Int -> Int
spiralToRing 0 = 0
spiralToRing x =
    let areas = hexNum <$> [0..]
     in length $ takeWhile (x >=) areas


spiralToHex :: Int -> Hex
spiralToHex 0 = origin
spiralToHex n =
    let r = radius n
        previousRings = hexNum (r - 1)
     in (ring r origin)!!(n - previousRings)


-- |Distance in number of hexagonal tiles.
dist :: Hex -> Hex -> Int
dist a b = (sum . abs $ a - b) `div` 2


-- |Hexagons which are within "range" tiles of the reference tile.
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


nearest :: V3 Double -> Hex
nearest v
  -- To prevent rounding errors from violating the constant that the
  -- sum of the components of a hex coordinate must equal zero, we
  -- must first determine which of the x, y, or z components suffered
  -- the most rounding error. Then, ensure that that component be
  -- nudged in the direction that ensures the entire coordinate is
  -- correct overall.
  | dx > dy && dx > dz = V3 (-ry-rz) ry rz
  | dy > dz            = V3 rx (-rx-rz) rz
  | otherwise          = V3 rx ry (-rx-ry)
      where V3 dx dy dz = v - (fromIntegral . round <$> v)
            V3 rx ry rz = round <$> v


lerp :: Double -> Double -> Double -> Double
lerp a b t = a + (b - a) * t

lerpV :: V3 Double -> V3 Double -> Double -> V3 Double
lerpV (V3 x y z) (V3 a b c) t =
    V3 (lerp x a t) (lerp y b t) (lerp z c t)

lerpHex :: Hex -> Hex -> Double -> Hex
lerpHex beg end = nearest . (lerpV begV endV) 
    where begV = fromIntegral <$> beg
          endV = fromIntegral <$> end


getPath :: Hex -> Hex -> [Hex]
getPath beg end = lerpHex beg end <$> [i / d | i <- [0..d]]
    where d = fromIntegral $ dist beg end


hexToSpiral :: Hex -> Int
hexToSpiral (V3 0 0 0) = 0
hexToSpiral tile = hexNum (radius - 1) + ringOverflow
    where radius    = dist tile origin
          outerRing = ring radius origin
          ringOverflow =
              case tile `elemIndex` outerRing of 
                Just x  -> x
                Nothing -> error "unknown error in hexToSpiral function"


-- |Create a Hex "Tree" that contains loops.
mkHexGraph :: [Bool] -> Hex -> Tree Hex
mkHexGraph blocked root =
    let isBlocked tile = blocked !! hexToSpiral tile
        reachable      = filter isBlocked $ neighbors root
     in Node root (mkHexGraph blocked <$> reachable)

-- |Create a hex tree.
mkHexTree :: [Bool] -> Hex -> Tree Hex
mkHexTree = _mkHexTree []
_mkHexTree visited blocked root =
    let isBlocked tile = (blocked !! hexToSpiral tile) || (tile `elem` visited)
        reachable      = filter isBlocked $ neighbors root
        visited'       = root : reachable ++ visited
     in Node root (_mkHexTree visited' blocked <$> reachable)


-- |Breadth first search, the Intro to Algorithms classic.
bfs :: [Bool] -> Hex -> [(Hex, Int)]
bfs blocked center = _bfs [] [(center, 0)] blocked


-- Breadth first search requires two bits of state: a list of visited tiles,
-- and a queue of hexes and their distance which have not been processed yet.
--
-- params:
--     visited - ensures that the same tiles aren't searched and returned
--               repeatedly
--     q       - pattern matched as ((q, k):qs), this function can only process
--               one item at a time, but multiple items are returned for a new
--               traversible tile.
--     k       - distance from the root; must be associated with each queued
--               tile when it is added to the queue because the function has no
--               other way of knowing when the item was added to the queue.
_bfs :: [Hex] -> [(Hex, Int)] -> [Bool] -> [(Hex, Int)]
_bfs visited [] blocked = []
_bfs visited ((q, k):qs) blocked
  | hexToSpiral q > length blocked = [] -- check needed for next guard
  | blocked!!hexToSpiral q = _bfs (q:visited) qs blocked
  | q `elem` visited       = _bfs   visited   qs blocked
  | otherwise = let mkPair tile = (tile, k + 1)
                    qs'         = qs ++ (mkPair <$> neighbors q)
                    visited'    = q : visited
                 in (q, k) : _bfs visited' qs' blocked


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


hasLineOfSight :: [Bool] -> Hex -> Hex -> Bool
hasLineOfSight blocked beg end =
    any (\i -> blocked!!i) $ hexToSpiral <$> getPath beg end


data MapDims = MapDims { minX :: Int, maxX :: Int
                       , minY :: Int, maxY :: Int
                       } deriving (Show, Eq)

wrapAround :: MapDims -> Hex -> Hex
wrapAround  mapDims tile = _wrapAround mapDims $ _wrapAround mapDims tile
_wrapAround mapDims tile
  | q < minX mapDims = fromOddr $ V2 (maxX mapDims) r
  | q > maxX mapDims = fromOddr $ V2 (minX mapDims) r
  | r < minY mapDims = fromOddr $ V2 q (maxY mapDims)
  | r > maxY mapDims = fromOddr $ V2 q (minY mapDims)
  | otherwise        = tile
    where V2 q r = toOddr tile
