-- Tests for the game.
module Main
  ( main
  ) where

import qualified Data.Matrix 

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import MovementTests
import PropertyTests
import Sokoban

defaultLevel :: Level 
defaultLevel = MkLevel {levelNum = -1, name = "default name", env = Data.Matrix.fromLists [[emptyCell]], trashCount = -1, exit = False, selectlvl = False}

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, movementTests, propertyTests]

test1 =
  testCase "Get count of trash in level" $
  (getTrashCount
     (fromCells
        [ [emptyCell, emptyCell, wallCell, emptyCell]
        , [emptyCell, trashCell, trashCell, wallCell]
        , [emptyCell, trashCell, playerCell, wallCell]
        , [wallCell, wallCell, emptyCell, emptyCell]
        ])) @?=
  3

test2 =
  testCase "Level is complete" $
  (isLevelComplete (defaultLevel {trashCount = 0})) @?= True

test3 =
  testCase "Level is NOT complete" $
  (isLevelComplete (defaultLevel {trashCount = 1})) @?= False

unitTests = testGroup "Game state tests" [test1, test2, test3]
