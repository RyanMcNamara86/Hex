module Traversal
    ( mkHexGraph
    , mkHexTree
    , bfs
    , hasLineOfSight
    , MapDims (..)
    , wrapAround
    ) where


import Basic
import Conversion
import Lerp
import Ring (hexToSpiral)

import Data.Tree

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
