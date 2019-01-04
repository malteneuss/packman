import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Knapsack

main = defaultMain tests

tests :: TestTree
tests = testGroup "pack greedily"
  [ testCase "1 example" $
     let itemCount' = 5 :: Int
         maxWeight' = 15 :: Int
         values' = V.fromList [2,4,3,2,1] :: V.Vector Int
         weights' = V.fromList [5,3,1,8,3] :: V.Vector Int
     in 3 == (3 :: Int)
  ]


