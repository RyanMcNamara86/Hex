module Hex.Traversal
    ( mkHexGraph
    , mkHexTree
    , bfs
    , hasLineOfSight
    , MapDims (..)
    , wrapAround
    , blockedTileDecorator 
    , mapSizeDecorator 
    , wrapDecorator 
    , heuristic
    , aStar
    , Path
    , Heuristic
    ) where

import Data.List
import Data.Tree
import Hex.Basic
import Hex.Conversion
import Hex.Lerp
import Hex.Ring (hexToSpiral)
import Linear.V2


-- Required to implement wrapAround.
data MapDims = MapDims { minX :: Int, maxX :: Int
                       , minY :: Int, maxY :: Int
                       } deriving (Show, Eq)


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


hasLineOfSight :: [Bool] -> Hex -> Hex -> Bool
hasLineOfSight blocked beg end =
    any (\i -> blocked!!i) $ hexToSpiral <$> getPath beg end


wrapAround :: MapDims -> Hex -> Hex
wrapAround  mapDims tile = _wrapAround mapDims $ _wrapAround mapDims tile
_wrapAround mapDims tile
  | q < minX mapDims = fromOddr $ V2 (maxX mapDims) r
  | q > maxX mapDims = fromOddr $ V2 (minX mapDims) r
  | r < minY mapDims = fromOddr $ V2 q (maxY mapDims)
  | r > maxY mapDims = fromOddr $ V2 q (minY mapDims)
  | otherwise        = tile
    where V2 q r = toOddr tile


--------------------------------------------------------------------------------
--           Breadth-First Search algorithm and helper functions.             --
--------------------------------------------------------------------------------

-- |Breadth first search, the Intro to Algorithms classic.
bfs :: [Bool] -> Hex -> [(Hex, Int)]
bfs blocked center = _bfs [] [(center, 0)] blocked


{-
   Breadth first search requires two bits of state: a list of visited tiles,
   and a queue of hexes and their distance which have not been processed yet.
  
   params:
       visited - ensures that the same tiles aren't searched and returned
                 repeatedly
       q       - pattern matched as ((q, k):qs), this function can only process
                 one item at a time, but multiple items are returned for a new
                 traversible tile.
       k       - distance from the root; must be associated with each queued
                 tile when it is added to the queue because the function has no
                 other way of knowing when the item was added to the queue.
-}
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


--------------------------------------------------------------------------------
--                A* search algorithm and helper functions                    --
--------------------------------------------------------------------------------

type Heuristic = Hex -> Path -> Int
type Path = [Hex]

{- |Some decorators to create a neighbor function. The reason the neighbor
    function doesn't just use the standard adjacent hexes is because A* is
    affected by map topology. If your map wraps around the top or bottom, or
    has a maximum/minimum length/height, or some other logic, this can be
    embedded inside the neighbor function.
-}

-- |Adjacent hexes which are not marked as blocked.
blockedTileDecorator :: [Bool] -> (Hex -> [Hex]) -> (Hex -> [Hex])
blockedTileDecorator blocked neighborFn =
    let n = length blocked
        isUnblocked tile = index >= n || not (blocked !! index)
            where index = hexToSpiral tile
     in \tile -> filter isUnblocked (neighborFn tile)

-- |Adjacent hexes such that the in-bounds map space is at most "limit" hexes,
-- not including the origin / center.
mapSizeDecorator :: Int -> (Hex -> [Hex]) -> (Hex -> [Hex])
mapSizeDecorator limit fn = \hex -> filter (\x -> hexToSpiral x <= limit) (fn hex)

wrapDecorator :: MapDims -> (Hex -> [Hex]) -> (Hex -> [Hex])
wrapDecorator dims fn = \tile -> wrapAround dims <$> fn tile


{- |A* search (A-Star).
  
   Please note that this implementation lets you can walk into a wall, but not
   through it. In other words, if the end point is blocked, aStar will still
   find a path to it.
-}


-- |Version of distance that takes into account how many tiles have already
-- been travelled.
heuristic :: Heuristic
heuristic target (x:xs) = dist target x + length xs

aStar :: Hex -> Hex -> Heuristic -> (Hex -> [Hex]) -> Path
aStar beg end heuristic neighborFn = _aStar end [[beg]] [] heuristic neighborFn 

_aStar ::  Hex           -- ^end: the tile you want to get to
       -> [Path]         -- ^open: tiles that can still be searched from
       -> [Hex]          -- ^closed: tiles which are known dead-ends
       -> Heuristic      -- ^h: the heuristic function
       -> (Hex -> [Hex]) -- ^neighborFn: get neighboring hexes
       -> Path           -- ^the path from A to B, avoiding any obstacles
_aStar end [  ] closed heuristic neighborFn = []
_aStar end open closed heuristic neighborFn
  | q == end                 = reverse $ q:qs
  | q `elem` closed          = _aStar end paths  (q:closed) heuristic neighborFn
  | otherwise                = _aStar end paths' (q:closed) heuristic neighborFn
    where (q:qs):paths = _pop heuristic open end
          paths' = (++) paths $ (\x -> x:q:qs) <$> neighborFn q

-- This sort function is used in place where "pop" would normally be used in an
-- imperative implementation of A* search.
_pop :: Heuristic -> [Path] -> Hex -> [Path]
_pop heuristic open end = sortBy hsort open
    where hsort xs ys = heuristic end xs `compare` heuristic end ys
