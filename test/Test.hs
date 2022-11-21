module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Sokoban 
import qualified Data.Matrix 


main :: IO ()
main = defaultMain tests 

tests :: TestTree
tests = testGroup "Tests" [unitTests]


level1 = Data.Matrix.fromLists [[emptyCell , emptyCell, wallCell  , emptyCell]
                                 ,[emptyCell , trashCell, trashCell , wallCell]
                                 ,[emptyCell , trashCell, playerCell, wallCell]
                                 ,[wallCell  , wallCell , wallCell  , emptyCell]
                                 ]
level1Loc = (3, 3)

test1 = testCase "Movement to an empty cell" $ (isValidMove LeftMv level1Loc level1) @?= True
test2 = testCase "Movement to a wall" $ (isValidMove RightMv level1Loc level1) @?= False

unitTests = testGroup "Unit tests"
  [ 
    test1,
    test2
  ]
