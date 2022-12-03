-- Tests for the game.
module Main
  ( main
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Matrix
import Sokoban

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

level1 =
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, wallCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell]
    , [emptyCell, trashCell, playerCell, wallCell]
    , [wallCell, wallCell, emptyCell, emptyCell]
    ]

level1Loc = (3, 3)

level2 =
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, wallCell]
    , [emptyCell, emptyCell, emptyCell]
    , [emptyCell, trashCell, playerCell]
    ]

level2Loc = (3, 3)

level3 =
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
    , [wallCell, wallCell, playerCell, emptyCell, emptyCell]
    , [wallCell, wallCell, emptyCell, emptyCell, emptyCell]
    ]

level3Loc = (4, 3)

level4 =
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, wallCell]
    , [emptyCell, emptyCell, emptyCell]
    , [stashCell, trashCell, playerCell]
    ]

level4Loc = (3, 3)

level5 =
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, playerCell, emptyCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
    , [wallCell, wallCell, stashCell, emptyCell, emptyCell]
    , [wallCell, wallCell, emptyCell, emptyCell, emptyCell]
    ]

level5Loc = (1, 3)

level6 =
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, wallCell]
    , [playerCell, emptyCell, emptyCell]
    , [stashCell, trashCell, emptyCell]
    ]

level6Loc = (2, 1)

test1 =
  testCase "Validate movement to an empty cell" $
  (isValidMove DownMv level1Loc level1) @?= ValidMov

test2 =
  testCase "Validate movement to a wall" $
  (isValidMove RightMv level1Loc level1) @?= InvalidMov

test3 =
  testCase "Validate movement to a trash cell" $
  (isValidMove LeftMv level1Loc level1) @?= ValidMov

test4 =
  testCase "Move to an empty cell" $
  (move UpMv level2) @?=
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, wallCell]
    , [emptyCell, emptyCell, playerCell]
    , [emptyCell, trashCell, emptyCell]
    ]

test5 =
  testCase "Move and push a trash object" $
  (move LeftMv level2) @?=
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, wallCell]
    , [emptyCell, emptyCell, emptyCell]
    , [trashCell, playerCell, emptyCell]
    ]

test6 = testCase "Move out of bounds" $ (move DownMv level2) @?= level2

test7 = testCase "Move into wall" $ (move LeftMv level3) @?= level3

test8 =
  testCase "Recursive push" $
  (move UpMv level3) @?=
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, trashCell, emptyCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
    , [emptyCell, trashCell, playerCell, wallCell, emptyCell]
    , [wallCell, wallCell, emptyCell, emptyCell, emptyCell]
    , [wallCell, wallCell, emptyCell, emptyCell, emptyCell]
    ]

test9 =
  testCase "Push trash into stash" $
  (move LeftMv level4) @?=
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, wallCell]
    , [emptyCell, emptyCell, emptyCell]
    , [stashCell, playerCell, emptyCell]
    ]

test10 =
  testCase "Recursive push of trash into stash" $
  (move DownMv level5) @?=
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]
    , [emptyCell, trashCell, playerCell, wallCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell, emptyCell]
    , [wallCell, wallCell, stashCell, emptyCell, emptyCell]
    , [wallCell, wallCell, emptyCell, emptyCell, emptyCell]
    ]

test11 = 
  testCase "Get amount of trash in level" $
  (getTrashCount level1) @?= 3

test12 =
  testCase "Player moves into stash" $
  (move DownMv level6) @?=
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, wallCell]
    , [emptyCell, emptyCell, emptyCell]
    , [Cell {gameObject=Player, background=Stash}, trashCell, emptyCell]
    ]

test13 =
  testCase "Player pushes trash into stash" $
  move LeftMv (move DownMv (move RightMv (move RightMv level6))) @?=
  Data.Matrix.fromLists
    [ [emptyCell, emptyCell, wallCell]
    , [emptyCell, emptyCell, emptyCell]
    , [stashCell, playerCell, emptyCell]
    ]


unitTests =
  testGroup
    "Unit tests"
    [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13]
