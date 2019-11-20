module Ring
    ( ring
    , spiralToRing
    , spiralToHex
    , hexToSpiral
    ) where

import Basic
import CardinalDirections
import Lerp (getPath)

import Data.List


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


hexToSpiral :: Hex -> Int
hexToSpiral (V3 0 0 0) = 0
hexToSpiral tile = hexNum (radius - 1) + ringOverflow
    where radius    = dist tile origin
          outerRing = ring radius origin
          ringOverflow =
              case tile `elemIndex` outerRing of 
                Just x  -> x
                Nothing -> error "unknown error in hexToSpiral function"
