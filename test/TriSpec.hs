module TriSpec where

import Data.Tree
import Test.Hspec
import Test.QuickCheck
import Test.Invariant

import Linear.V3
import Linear.V2

import Tri


main :: IO ()
main = hspec spec

evenExamples = [V3 0 0 0, V3 1 0 1   , V3 0 (-1) (-1), V3 (-2) 2 2]
oddExamples  = [V3 0 1 0, V3 1 1 (-1), V3 0 2 1      , V3 3 2 2   ] 

spec :: Spec
spec = parallel $ do
    describe "isEven" $ do
        it "should be True for all even triangles" $ do
            all isEven evenExamples `shouldBe` True

        it "should be False for all odd triangles" $ do
            any isEven oddExamples `shouldBe` False


    describe "isOdd" $ do
        it "should be False for all even triangles" $ do
            any isOdd evenExamples `shouldBe` False

        it "should be True for all odd triangles" $ do
            all isOdd oddExamples `shouldBe` True


    describe "dist" $ do
        it "should be 0 for the origin." $ do
            dist origin origin `shouldBe` 0

        it "should be 1 for all tris of distance 1 from the origin." $ do
            dist origin (V3 0 0 1) `shouldBe` 1
            dist origin (V3 0 1 0) `shouldBe` 1
            dist origin (V3 1 0 0) `shouldBe` 1

        it "should be 2 for all tris of distance 2 from the origin." $ do
            dist origin (V3 1 0 (-1)) `shouldBe` 2
            dist origin (V3 0 1 (-1)) `shouldBe` 2
            dist origin (V3 (-1) 1 0) `shouldBe` 2

            dist origin (V3 (-1) 0 1) `shouldBe` 2
            dist origin (V3 0 (-1) 1) `shouldBe` 2
            dist origin (V3 1 (-1) 0) `shouldBe` 2

        it "should be 1 for all tris of distance 1 from the origin." $ do
            dist origin (V3 1 (-1) 1) `shouldBe` 3
            dist origin (V3 2 (-1) 0) `shouldBe` 3
            dist origin (V3 2 0 (-1)) `shouldBe` 3

            dist origin (V3 1 1 (-1)) `shouldBe` 3
            dist origin (V3 0 2 (-1)) `shouldBe` 3
            dist origin (V3 (-1) 2 0) `shouldBe` 3

            dist origin (V3 (-1) 1 1) `shouldBe` 3
            dist origin (V3 (-1) 0 2) `shouldBe` 3
            dist origin (V3 0 (-1) 2) `shouldBe` 3

        it "should still provide the correct answer when measuring the distance that isn't the origin" $ do
            dist (V3 (-1) 2 (-1)) (V3 1 1 (-2))    `shouldBe` 4
            dist (V3 (-3) 2   1 ) (V3 2 0 (-1))    `shouldBe` 9
            dist (V3 (-1) (-1) 3) (V3 (-1) (-1) 2) `shouldBe` 1


    describe "neighbors" $ do
        it "should only ever return 3 neighbors" $ property $
            (\x y z -> length (neighbors $ V3 x y z) == 3)

        it "correctly identifies the neighbors of V3 0 0 0" $ do
            neighbors origin `shouldContain` [V3 1 0 0]
            neighbors origin `shouldContain` [V3 0 1 0]
            neighbors origin `shouldContain` [V3 0 0 1]

        it "correctly identifies the neighbors of V3 1 0 0" $ do
            neighbors (V3 1 0 0) `shouldContain` [V3 0 0 0]
            neighbors (V3 1 0 0) `shouldContain` [V3 1 (-1) 0]
            neighbors (V3 1 0 0) `shouldContain` [V3 1 0 (-1)]

        it "correctly identifies the neighbors of V3 1 1 1" $ do
            neighbors (V3 1 (-1) 0) `shouldContain` [V3 1 0 0]
            neighbors (V3 1 (-1) 0) `shouldContain` [V3 2 (-1) 0]
            neighbors (V3 1 (-1) 0) `shouldContain` [V3 1 (-1) 1]


    describe "diagonals" $ do
        it "should return the correct diagonals for an even-parity triangle" $ do
            diagonals (V3 (-1) 0 1) `shouldContain` [V3 (-1) (-1) 2]
            diagonals (V3 (-1) 0 1) `shouldContain` [V3 0 (-1) 2]
            diagonals (V3 (-1) 0 1) `shouldContain` [V3 0 (-1) 1]

            diagonals (V3 (-1) 0 1) `shouldContain` [V3 0 0 0]
            diagonals (V3 (-1) 0 1) `shouldContain` [V3 0 1 0]
            diagonals (V3 (-1) 0 1) `shouldContain` [V3 (-1) 1 0]

            diagonals (V3 (-1) 0 1) `shouldContain` [V3 (-2) 1 1]
            diagonals (V3 (-1) 0 1) `shouldContain` [V3 (-2) 1 2]
            diagonals (V3 (-1) 0 1) `shouldContain` [V3 (-2) 0 2]

        it "should return the correct diagonals for an odd-parity triangle" $ do
            diagonals (V3 0 1 0) `shouldContain` [V3 1 0 0]
            diagonals (V3 0 1 0) `shouldContain` [V3 1 0 (-1)]
            diagonals (V3 0 1 0) `shouldContain` [V3 1 1 (-1)]

            diagonals (V3 0 1 0) `shouldContain` [V3 0 2 (-1)]
            diagonals (V3 0 1 0) `shouldContain` [V3 (-1) 2 (-1)]
            diagonals (V3 0 1 0) `shouldContain` [V3 (-1) 2 0]

            diagonals (V3 0 1 0) `shouldContain` [V3 (-1) 1 1]
            diagonals (V3 0 1 0) `shouldContain` [V3 (-1) 0 1]
            diagonals (V3 0 1 0) `shouldContain` [V3 0 0 1]


--    describe "inRange" $ do
--        it "should produce a number of tiles predicted by triNum" $ property $
--            (\n -> triNum n == (length $ inRange origin n))
--
--        it "should have all the tiles for range 2" $ do
--            inRange origin 3 `shouldContain` [V3 0 0 0]
--            inRange origin 3 `shouldContain` [V3 1 0 0]
--            inRange origin 3 `shouldContain` [V3 0 1 0]
--            inRange origin 3 `shouldContain` [V3 0 0 1]
--            inRange origin 3 `shouldContain` [V3 1 0 (-1)]
--            inRange origin 3 `shouldContain` [V3 0 1 (-1)]
--            inRange origin 3 `shouldContain` [V3 (-1) 1 0]
--            inRange origin 3 `shouldContain` [V3 (-1) 0 1]
--            inRange origin 3 `shouldContain` [V3 0 (-1) 1]
--            inRange origin 3 `shouldContain` [V3 1 (-1) 0]


    describe "r120" $ do
        it "should rotate triangles around the origin" $ do
            r120 origin (V3 1 (-1) 1) 1 `shouldBe` (V3 1 1 (-1))
            r120 origin (V3 1 (-1) 1) 2 `shouldBe` (V3 (-1) 1 1)
            r120 origin (V3 1 (-1) 1) 3 `shouldBe` (V3 1 (-1) 1)

            r120 origin (V3 1 0 0) 1 `shouldBe` (V3 0 1 0)
            r120 origin (V3 1 0 0) 2 `shouldBe` (V3 0 0 1)
            r120 origin (V3 1 0 0) 3 `shouldBe` (V3 1 0 0)

        it "should rotate triangles around a non-origin point" $ do
            r120 (V3 1 0 (-1)) (V3 1 0 0) 1 `shouldBe` (V3 2 0 (-1))
            r120 (V3 1 0 (-1)) (V3 1 0 0) 2 `shouldBe` (V3 1 1 (-1))
            r120 (V3 1 0 (-1)) (V3 1 0 0) 3 `shouldBe` (V3 1 0 0)
