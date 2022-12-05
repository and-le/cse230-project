module PropertyTests
  ( propertyTests
  ) where

import qualified Data.Matrix (fromList, setElem)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Sokoban

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property tests"
    [ testProperty
        "Movement preserves the environment size"
        prop_movePreservesEnvironmentSize
    , testProperty
        "Movement preserves the player's existence"
        prop_movePreservesPlayer
    , testProperty
        "Movement preserves the stash's existence"
        prop_movePreservesStash
    , testProperty
        "Movement preserves the walls' existence"
        prop_movePreservesWalls
    ]

prop_movePreservesEnvironmentSize :: Movement -> EnvWrapper -> Property
prop_movePreservesEnvironmentSize mv envWrapper =
  property ((rows', cols') == (rows, cols))
  where
    environment = wrappedEnv envWrapper
    rows = getNumRows environment
    cols = getNumCols environment
    environment' = move mv environment
    rows' = getNumRows environment'
    cols' = getNumCols environment'

prop_movePreservesPlayer :: Movement -> EnvWrapper -> Property
prop_movePreservesPlayer mv envWrapper =
  (countNumPlayers environment == 1) ==> countNumPlayers environment' == 1
  where
    environment = wrappedEnv envWrapper
    environment' = move mv environment

prop_movePreservesStash :: Movement -> EnvWrapper -> Property
prop_movePreservesStash mv envWrapper =
  property (countNumStashes environment == countNumStashes environment')
  where
    environment = wrappedEnv envWrapper
    environment' = move mv environment

prop_movePreservesWalls :: Movement -> EnvWrapper -> Property
prop_movePreservesWalls mv envWrapper =
  property (countNumWalls environment == countNumWalls environment')
  where
    environment = wrappedEnv envWrapper
    environment' = move mv environment

-- Generator for Movement
instance Arbitrary Movement where
  arbitrary = elements [UpMv, RightMv, DownMv, LeftMv]

-- Generator for Cells. 
-- Only generates trashCell, wallCell, stashCell, and emptyCell, since playerCell is
-- generally one-of-a-kind for a given level.
instance Arbitrary Cell where
  arbitrary = elements [trashCell, wallCell, stashCell, emptyCell]

-- Wrapper data type for the Environment, required to define
-- an Arbitrary instance.
data EnvWrapper =
  EnvWrapper
    { wrappedEnv :: Environment
    }
  deriving (Show)

-- Arbitrary instance for EnvWrapper, 
-- functionally used as arbitrrary instance for Environment
instance Arbitrary EnvWrapper where
  arbitrary = sized genEnvironment

-- Generates a random Environment with the given size.
-- The size parameter is taken to be the number of rows in the grid. 
-- The size will be set to be 2 if a value smaller than 2 is provided.
-- The number of columns is then chosen randomly to be in the range [2, rows].
genEnvironment :: Int -> Gen EnvWrapper
genEnvironment numRows = do
  let numRows' = max 2 numRows 
  numCols <- chooseInt (2, max 2 numRows')
      -- Generate a random location for the player
  playerLoc@(playerRow, playerCol) <- genLocation numRows' numCols
      -- Generate a flattened matrix of cells
  cells <- vectorOf (numRows' * numCols) (arbitrary :: Gen Cell)
      -- Place the player into the environment
  let grid =
        Data.Matrix.setElem
          playerCell
          playerLoc
          (Data.Matrix.fromList numRows' numCols cells)
  return (EnvWrapper grid)

-- Generates a random (row, column) location such that row is in the range [1, maxRow]
-- and column is in the range [1, maxCol].
genLocation :: Int -> Int -> Gen Location
genLocation maxRow maxCol = do
  row <- chooseInt (1, maxRow)
  col <- chooseInt (1, maxCol)
  return (row, col)
