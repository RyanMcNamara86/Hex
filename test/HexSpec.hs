module HexSpec where

import Data.Tree
import Test.Hspec
import Test.QuickCheck
import Test.Invariant

import Linear.V3
import Linear.V2

import Hex


-- The Haskell type checker can't figure out the type of this line in context,
-- so it has to be broken out from where it is used in the function below.
isNeighborly :: Int    -- ^x
             -> Int    -- ^y
             -> Int    -- ^z
             -> [Hex]  -- ^ns
             -> Bool
isNeighborly x y z ns = (V3 x y z) `elem` ns

prop_neighborly x y z = all (isNeighborly x y z) ns
    where ns = neighbors <$> neighbors (V3 x y z)

prop_diagonal x y z = all (isNeighborly x y z) ds
    where ds = diagonals <$> diagonals (V3 x y z)

isValid :: Hex -> Bool
isValid hex = sum hex == 0

instance Arbitrary MapDims where
    arbitrary = do
        x0 <- arbitrary
        y0 <- arbitrary
        x1 <- arbitrary
        y1 <- arbitrary
        return $ MapDims (min x0 x1) (max x0 x1) (min y0 y1) (max y0 y1)


-- These are used to test maps with blocked tiles.
blocked = [ False, False, True , False,
            False, True , False, False,
            True , True , True , True ,
            False, True , True , False,
            False, True , True ]

-- There's not a pretty way to write this.
onlyBlocked = [  2,  5,  8, 10, 11, 13, 14, 17
              , 18, 21, 26, 27, 33, 40, 65 ]
ifBlocked i = if i `elem` onlyBlocked then True else False 
blocked2 = ifBlocked <$> [0..90]

neighborFn :: Hex -> [Hex]
neighborFn = let f = mapSizeDecorator 89
                 g = blockedTileDecorator blocked2
                 h = neighbors
              in f $ g $ h


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "Hex neighbors" $ do
        it "correctly identifies it's neighbors" $ do
            (neighbors (V3 1 0 0)) `shouldContain` [V3   1   1  (-1)]
            (neighbors (V3 0 2 0)) `shouldContain` [V3   1   2  (-1)]
            (neighbors (V3 0 0 4)) `shouldContain` [V3   1 (-1)   4 ]
            (neighbors (V3 0 5 0)) `shouldContain` [V3   0   4    1 ]
            (neighbors (V3 8 0 0)) `shouldContain` [V3   7   0    1 ]
            (neighbors (V3 0 9 0)) `shouldContain` [V3 (-1) 10    0 ]
        it "is a neighbor of its neighbors" $ property $ prop_neighborly
        it "is diagonal to each of its diagonals" $ property $ prop_diagonal


    describe "Hex ring" $ do
        it "should return the origin for ring 0" $ do
            ring 0 origin `shouldBe` [origin]

        it "should return the all hex coordinates for ring 1" $ do
            ring 1 origin `shouldContain` [V3 (-1) 1 0]
            ring 1 origin `shouldContain` [V3 (-1) 0 1]
            ring 1 origin `shouldContain` [V3 0 (-1) 1]
            ring 1 origin `shouldContain` [V3 1 (-1) 0]
            ring 1 origin `shouldContain` [V3 1 0 (-1)]
            ring 1 origin `shouldContain` [V3 0 1 (-1)]

        it "should return the correct number of tiles for that ring" $ property $
            \x -> if x > 0 then length (ring x origin) == hexNum x - (hexNum (x - 1)) else True


    describe "Hex spiralToRing" $ do
        it "should produce the right answer" $ do
            spiralToRing  0 `shouldBe` 0
            spiralToRing  1 `shouldBe` 1
            spiralToRing  2 `shouldBe` 1
            spiralToRing  3 `shouldBe` 1
            spiralToRing  4 `shouldBe` 1
            spiralToRing  5 `shouldBe` 1
            spiralToRing  6 `shouldBe` 1
            spiralToRing  7 `shouldBe` 2
            spiralToRing 18 `shouldBe` 2
            spiralToRing 19 `shouldBe` 3


    describe "Hex spiralToHex" $ do
        it "should return the the correct first 7 hexes" $ do
            spiralToHex 0 `shouldBe` origin
            spiralToHex 1 `shouldBe` V3 (-1) 1 0
            spiralToHex 2 `shouldBe` V3 (-1) 0 1
            spiralToHex 3 `shouldBe` V3 0 (-1) 1
            spiralToHex 4 `shouldBe` V3 1 (-1) 0
            spiralToHex 5 `shouldBe` V3 1 0 (-1)
            spiralToHex 6 `shouldBe` V3 0 1 (-1)


    describe "Hex hexToSpiral" $ do
        it "should return the the correct first 7 hexes" $ do
            hexToSpiral origin        `shouldBe` 0
            hexToSpiral (V3 (-1) 1 0) `shouldBe` 1
            hexToSpiral (V3 (-1) 0 1) `shouldBe` 2
            hexToSpiral (V3 0 (-1) 1) `shouldBe` 3
            hexToSpiral (V3 1 (-1) 0) `shouldBe` 4
            hexToSpiral (V3 1 0 (-1)) `shouldBe` 5
            hexToSpiral (V3 0 1 (-1)) `shouldBe` 6


    describe "Hex cardinal directions" $ do
        it "dist is 5 for a hex 5 tiles from the origin" $ do
            let end =  right + right + rightup + right + rightup + origin
             in dist origin end `shouldBe` 5

        it "dist is 3 for a hex 3 tiles from the origin" $ do
            let end = left + leftdown + leftdown + origin 
             in dist origin end `shouldBe` 3

        it "dist is 3 for a hex 3 tiles from a hex that is not the origin" $ do
            let beg = left + left +leftup + origin
                end = leftdown + left + left + beg
             in dist beg end `shouldBe` 3

        it "hexes are correctly shifted by right" $
            right + origin `shouldBe` V3 1 (-1) 0

        it "hexes are correctly shifted by left"  $
            left + origin `shouldBe` V3 (-1) 1 0

        it "hexes are correctly shifted by leftup"  $
            leftup + origin `shouldBe` V3 0 1 (-1)

        it "hexes are correctly shifted by rightdown"  $
            rightdown + origin `shouldBe` V3 0 (-1) 1

        it "hexes are correctly shifted by rightup"  $
            rightup + origin `shouldBe` V3 1 0 (-1)

        it "hexes are correctly shifted by leftdown" $
            leftdown + origin `shouldBe` V3 (-1) 0 1


    describe "Hex.Movement lerp" $ do
        it "is monotonic increasing" $ property $ monotonicIncreasing' (lerp 0 1)
        it "is fixed by"             $ property $ fixedBy (lerp 0) 0
        it "is id between 0 and 1"   $ property $ (lerp 0 1) <=> id


    describe "Hex getPath" $ do
        it "Lerps between -4 4 0 and 3 -4 1 correctly" $ do
            let beg  = V3 (-4) 4 0
                end  = V3 3 (-4) 1
                path = [ V3 (-4) 4 0, V3 (-3) 3 0, V3 (-2) 2 0
                       , V3 (-1) 1 0, V3 0 0 0   , V3 0 (-1) 1
                       , V3 1 (-2) 1, V3 2 (-3) 1, V3 3 (-4) 1
                       ]
             in getPath beg end `shouldBe` path

        it "Lerps between -4 4 0 and 2 -3 1 correctly" $ do
            let beg  = V3 (-4) 4 0
                end  = V3 2 (-3) 1
                path = [ V3 (-4) 4 0, V3 (-3) 3 0, V3 (-2) 2 0
                       , V3 (-1) 1 0, V3 (-1) 0 1, V3 0 (-1) 1
                       , V3 1 (-2) 1, V3 2 (-3) 1
                       ]
             in getPath beg end `shouldBe` path


    describe "Hex inRange" $ do
        it "should return itself with a range of 0." $ do
            inRange origin 0 `shouldBe` [origin]

        it "should return valid cube coordinate hexes only" $ do
            all isValid (inRange origin 1) `shouldBe` True

        it "is monotonic-increasing" $ property $
            let f r = length (inRange origin r)
             in monotonicIncreasing f


    describe "Hex breadth first search ( _bfs )" $ do
        it "should produce 4 traversible tiles at distance 1" $ do
            let searched = take (hexNum 1) (bfs blocked origin)
            searched `shouldContain` [(V3 (-1) 1 0, 1)]
            searched `shouldContain` [(V3 0 (-1) 1, 1)]
            searched `shouldContain` [(V3 (-1) 1 0, 1)]
            searched `shouldContain` [(V3 0 1 (-1), 1)]

        it "should produce 7 traversible tiles at distance 2" $ do
            let filterDist2 = takeWhile (\(_, n) -> n <= 2)
                bfsResults  = bfs blocked origin
             in length bfsResults `shouldBe` 8


    describe "r60" $ do
        it "should rotate tiles along an axis" $ do
            -- TODO: add more examples
            r60 origin (V3 2 1 (-3)) (-1) `shouldBe` V3 (-1) 3 (-2)


    describe "wrap-around" $ do
        it "should be correct for a sample of predetermined scenarios" $ do
            let mapDims = MapDims (-2) 2 (-2) 2
                input  = fromOddr $ V2 (-3) 2
                output = fromOddr $ V2 2 2
             in wrapAround mapDims input `shouldBe` output

            let mapDims = MapDims (-3) 2 (-3) 2
                input  = fromOddr $ V2 (-4) 0
                output = fromOddr $ V2 2 0
             in wrapAround mapDims input `shouldBe` output

            let mapDims = MapDims (-2) 2 (-2) 2
                input  = fromOddr $ V2 (-2) 3
                output = fromOddr $ V2 (-2) (-2)
             in wrapAround mapDims input `shouldBe` output

        it "should never produce a tile outside the map bounds" $ property $
            \mapDims q r ->
                let tile     = fromOddr $ V2 q r
                    V2 q' r' = toOddr $ wrapAround mapDims tile
                 in q' >= minX mapDims && q' <= maxX mapDims &&
                    r' >= minY mapDims && r' <= maxY mapDims


    describe "A* Search" $ do
        it "should not find a path to tiles surrounded by blocked tiles" $ do
            aStar'  3 `shouldBe` (spiralToHex <$> [0, 3])
            aStar'  4 `shouldBe` (spiralToHex <$> [0, 4])

            let xs = aStar' 12 in head   xs `shouldBe` origin
            let xs = aStar' 12 in last   xs `shouldBe` spiralToHex 12
            let xs = aStar' 12 in length xs `shouldBe` 3

            aStar'  1 `shouldBe` (spiralToHex <$> [0, 1])
            aStar'  6 `shouldBe` (spiralToHex <$> [0, 6])
            aStar'  9 `shouldBe` (spiralToHex <$> [0, 6, 16, 15, 30, 29, 28, 48, 47, 46, 25, 24, 23, 9])
            aStar' 35 `shouldBe` (spiralToHex <$> [0, 1, 7, 36, 35])
            aStar' 34 `shouldBe` (spiralToHex <$> [0, 1, 7, 36, 35, 34])
            aStar' 56 `shouldBe` (spiralToHex <$> [0, 6, 16, 32, 55, 56])
            aStar' 36 `shouldBe` (spiralToHex <$> [0, 1, 7, 36])
                where aStar' = \x -> aStar origin (spiralToHex x) heuristic neighborFn
