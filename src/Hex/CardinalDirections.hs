module Hex.CardinalDirections
    ( right
    , left
    , leftup
    , leftdown
    , rightup
    , rightdown
    ) where

import Hex.Basic


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


