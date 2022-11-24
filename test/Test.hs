-- Tests for the game.
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
                                 ,[wallCell  , wallCell , emptyCell  , emptyCell]
                                 ]
level1Loc = (3, 3)

level2 = Data.Matrix.fromLists [[emptyCell , emptyCell, wallCell]
                                 ,[emptyCell , emptyCell, emptyCell]
                                 ,[emptyCell , trashCell, playerCell]
                                 ]
level2Loc = (3, 3)

level3 = Data.Matrix.fromLists [[emptyCell , emptyCell, emptyCell  , emptyCell, emptyCell]
                                 ,[emptyCell , trashCell, trashCell , wallCell, emptyCell]
                                 ,[emptyCell , trashCell, trashCell, wallCell, emptyCell]
                                 ,[wallCell  , wallCell , playerCell  , emptyCell, emptyCell]
                                 ,[wallCell  , wallCell , emptyCell  , emptyCell, emptyCell]
                                 ]
level3Loc = (4, 3)

test1 = testCase "Validate movement to an empty cell" $ (isValidMove DownMv level1Loc level1) @?= ValidMov
test2 = testCase "Validate movement to a wall" $ (isValidMove RightMv level1Loc level1) @?= InvalidMov
test3 = testCase "Validate movement to a trash cell" $ (isValidMove LeftMv level1Loc level1) @?= ValidMov
test4 = testCase "Move to an empty cell" $ (move UpMv level2) @?= Data.Matrix.fromLists [[emptyCell , emptyCell, wallCell]
                                                                  ,[emptyCell , emptyCell, playerCell]
                                                                  ,[emptyCell , trashCell, emptyCell]
                                                                  ]
test5 = testCase "Move and push a trash object" $ (move LeftMv level2) @?= Data.Matrix.fromLists [[emptyCell , emptyCell, wallCell]
                                                                  ,[emptyCell , emptyCell, emptyCell]
                                                                  ,[trashCell , playerCell, emptyCell]
                                                                  ]
test6 = testCase "Move out of bounds" $ (move DownMv level2) @?= level2
test7 = testCase "Move into wall" $ (move LeftMv level3) @?= level3
test8 = testCase "Recursive push" $ (move UpMv level3) @?= Data.Matrix.fromLists 
                                [[emptyCell , emptyCell, trashCell  , emptyCell, emptyCell]
                                 ,[emptyCell , trashCell, trashCell , wallCell, emptyCell]
                                 ,[emptyCell , trashCell, playerCell, wallCell, emptyCell]
                                 ,[wallCell  , wallCell , emptyCell  , emptyCell, emptyCell]
                                 ,[wallCell  , wallCell , emptyCell  , emptyCell, emptyCell]
                                 ]
                              
                                                                
unitTests = testGroup "Unit tests"
  [ 
    test1,
    test2,
    test3,
    test4,
    test5,
    test6,
    test7,
    test8
  ]
