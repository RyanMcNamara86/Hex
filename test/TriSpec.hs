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
spec = do
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
            dist (V3 (-1) 2 (-1)) (V3 1 1 (-2)) `shouldBe` 4
            dist (V3 (-3) 2   1 ) (V3 2 0 (-1)) `shouldBe` 9
            dist (V3 (-1) (-1) 3) (V3 (-1) (-1) 2) `shouldBe` 1

    --describe "Hex neighbors" $ do
        --it "correctly identifies it's neighbors" $ do
            --dist origin <$> (neighbors origin) `shouldBe` [1, 1, 1]
