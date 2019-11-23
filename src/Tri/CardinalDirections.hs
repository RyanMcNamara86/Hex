module Tri.CardinalDirections
    ( right
    , left
    , leftup
    , leftdown
    , rightup
    , rightdown
    ) where

import Tri.Basic
import Linear.V3


-- |The hex to the right of the reference hex on the left-right axis.
right :: Tri
right = V3 1 (-1) 0

-- |The hex to the left of the reference hex on the left-right axis.
left :: Tri
left = V3 (-1) 1 0

-- |The hex to the left and up of the reference hex on the NW-SE axis.
leftup :: Tri
leftup = V3 0 1 (-1)

-- |The hex to the right and down of the reference hex on the NW-SE axis.
rightdown :: Tri
rightdown = V3 0 (-1) 1

-- |The hex to the right and up of the reference hex on the SW-NE axis.
rightup :: Tri
rightup = V3 1 0 (-1)

-- |The hex to the left and down from the reference hex on the SW-NE axis.
leftdown :: Tri
leftdown = V3 (-1) 0 1


