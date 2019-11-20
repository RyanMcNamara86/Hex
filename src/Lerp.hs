module Lerp
    ( nearest
    , lerp
    , lerpV
    , lerpHex
    , getPath
    ) where

import Basic


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
