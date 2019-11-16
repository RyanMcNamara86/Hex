module Conversion where

import Linear.V3
import Linear.V2


{-| Conversion TO Cube Coordinates. -}

toOddr (V3 x y z) = V2 q z
    where q = x + (z - (z `mod` 2)) `div` 2

toEvenr (V3 x y z) = V2 q z
    where q = x + (z + (z `mod` 2)) `div` 2

toOddq (V3 x y z) = V2 x r
    where r = z + (x - (x `mod` 2)) `div` 2

toEvenq (V3 x y z) = V2 x r
    where r = z + (x + (x `mod` 2)) `div` 2

toAxial (V3 x y z) = V2 x z

toDubW (V3 x y z) = V2 (2 * x + z) z

toDubH (V3 x y z) = V2 x (2 * z + x)


{-| Conversion FROM Cube Coordinates.

   Note that, by convention, q and r stand for column and row
   to avoid confusion with rectangle-based systems.
-}

fromAxial (V2 q r) = V3 x y z
    where x = q
          y = -x-z
          z = r

fromEvenr (V2 q r) =
    let x = q - (r + (r `mod` 2)) `div` 2
        y = -x - z
        z = r
     in V3 x y z

fromOddr (V2 q r) =
    let x = q - (r - (r `mod` 2)) `div` 2
        y = -x - z
        z = r
     in V3 x y z

fromOddq (V2 q r) =
    let x = q
        y = -x - z
        z = r - (q - (q `mod` 2)) `div` 2
     in V3 x y z

fromEvenq (V2 q r) =
    let x = q
        y = -x - z
        z = r - (q + (q `mod` 2)) `div` 2
     in V3 x y z

fromDubH (V2 q r) =
    let x = q
        y = -x - z
        z = (r - q) `div` 2
     in V3 x y z

fromDubW (V2 q r) =
    let x = (q - r) `div` 2
        y = -x - z
        z = r 
     in V3 x y z
