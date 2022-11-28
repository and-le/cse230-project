-- The main file for the underlying logic of the game.
module Sokoban
  ( GameObject(..)
  , Background(..)
  , Cell(..)
  , Environment
  , Movement(..)
  , MoveStatus(..)
  , move
  , sampleLevel
  , emptyCell
  , playerCell
  , wallCell
  , trashCell
  , stashCell
  , isValidMove
  ) where

import qualified Data.Ix (inRange)
import Data.Matrix
import qualified Data.Vector (length)

-- An object that the player can interact with in the game
data GameObject
  = Player
  | Trash
  | Wall
  | Empty
  deriving (Eq, Show)

-- The appearance of a grid cell
data Background
  = Trashcan
  | Stash
  | EmptyBG
  deriving (Eq, Show)

-- An individual cell within a level, consisting of both a GameObject and a Background
data Cell =
  Cell
    { gameObject :: GameObject
    , background :: Background
    }
  deriving (Eq)

-- Override default show for Cells to make display more compact.
instance Show Cell where
  show (Cell gameObject background) = show gameObject ++ ":" ++ show background

-- The representation of an individual level
type Environment = Matrix Cell

-- The types of valid moves
data Movement
  = UpMv
  | RightMv
  | DownMv
  | LeftMv
  deriving (Eq, Show)

-- This could be replaced with a Boolean type, but 
-- we may add other MoveStatuses in the future.
data MoveStatus
  = ValidMov
  | InvalidMov
  deriving (Eq, Show)

-- Represents a (row, column) index in an Environment
type Location = (Int, Int)

-- level building
emptyCell :: Cell -- just for shorter code
emptyCell = Cell {gameObject = Empty, background = EmptyBG}

playerCell :: Cell -- cell with player
playerCell = Cell {gameObject = Player, background = EmptyBG}

wallCell :: Cell -- a wall
wallCell = Cell {gameObject = Wall, background = EmptyBG}

trashcanCell :: Cell -- starting point of the trash
trashcanCell = Cell {gameObject = Trash, background = Trashcan}

trashCell :: Cell -- a cell containing trash that is not the starting point
trashCell = Cell {gameObject = Trash, background = EmptyBG}

stashCell :: Cell -- an empty stash
stashCell = Cell {gameObject = Empty, background = Stash}

-- level definitions
sampleLevel :: Environment
sampleLevel =
  fromLists
    [ [emptyCell, emptyCell, wallCell, emptyCell]
    , [emptyCell, trashCell, trashCell, wallCell]
    , [emptyCell, trashCell, trashCell, playerCell]
    , [wallCell, wallCell, wallCell, emptyCell]
    ]

-- handle state change for motions
move :: Movement -> Environment -> Environment
move mv start =
  case isVal of
    ValidMov -> moveHelper mv loc start
    InvalidMov -> start -- return same state since not valid move
  where
    loc = getPlayerLocation start
    isVal = isValidMove mv loc start

-- Moves the object at the given location within the given environment in the specified direction.
-- Attempts to recursively move all pushable objects encountered by the initial move.
moveHelper :: Movement -> Location -> Environment -> Environment
moveHelper mv fromLoc env = newEnv'
  where
    toLoc = getLocationAfterMovement mv fromLoc
    fromElem = getCellAtLocation fromLoc env
    toElem = getCellAtLocation toLoc env
      -- The updated elements after performing the move:
      -- The backgrounds are currently kept the same.
      -- The object in the from-location replaces the object in the to-location.
      -- Trash should disappear if it is pushed into the stash
    fromElem' = Cell {gameObject = Empty, background = (background fromElem)}
    toElem' =
      Cell
        { gameObject =
            (if (containsStash toLoc env)
               then Empty
               else gameObject fromElem)
        , background = (background toElem)
        }
      -- If the object previously occupying the to-location is pushable, attempt to move it recursively.
    newEnv =
      if (gameObject toElem == Trash)
        then moveHelper mv toLoc env
        else env
    newEnv' = setElem toElem' toLoc (setElem fromElem' fromLoc newEnv)

-- Returns True if the given location in the environment contains a stash; False otherwise.
containsStash :: Location -> Environment -> Bool
containsStash loc env = background (getCellAtLocation loc env) == Stash

-- Returns the Cell at the given location in the environment.
getCellAtLocation :: Location -> Environment -> Cell
getCellAtLocation (row, col) env = getElem row col env

-- Returns a MoveStatus for the given movement with respect to the given location for the 
-- current environment; ValidMov for valid moves, InvalidMov otherwise.
isValidMove :: Movement -> Location -> Environment -> MoveStatus
isValidMove mv loc env =
  if boundsCheck
    then ValidMov
    else InvalidMov
  where
    destLoc = getLocationAfterMovement mv loc
    destCell = getCellAtLocation destLoc env
    boundsCheck =
      (isWithinEnvironment loc env) &&
      (isWithinEnvironment destLoc env) &&
      (not (containsWall destCell)) &&
      (not (containsPlayer destCell)) &&
      (containsEmptySpace destCell ||
       (containsTrash destCell && (isValidMove mv destLoc env == ValidMov)))

-- Returns True if the given location is within the environment; False otherwise.
isWithinEnvironment :: Location -> Environment -> Bool
isWithinEnvironment (row, col) env =
  (Data.Ix.inRange rowRange row) && (Data.Ix.inRange colRange col)
  where
    rowRange = (1, getNumRows env)
    colRange = (1, getNumCols env)

-- Returns the Location that would result if the given Movement is applied to the current location.
getLocationAfterMovement :: Movement -> Location -> Location
getLocationAfterMovement UpMv (row, col) = (row - 1, col)
getLocationAfterMovement RightMv (row, col) = (row, col + 1)
getLocationAfterMovement DownMv (row, col) = (row + 1, col)
getLocationAfterMovement LeftMv (row, col) = (row, col - 1)

-- Returns True if the given Cell contains a Wall, False otherwise.
containsWall :: Cell -> Bool
containsWall (Cell Wall _) = True
containsWall _ = False

-- Returns True if the given Cell contains a Player; False otherwise.
containsPlayer :: Cell -> Bool
containsPlayer (Cell Player _) = True
containsPlayer _ = False

-- Returns True if the given Cell contains an Empty space; False otherwise.
containsEmptySpace :: Cell -> Bool
containsEmptySpace (Cell Empty _) = True
containsEmptySpace _ = False

-- Returns True if the given Cell contains a Trash; False otherwise.
containsTrash :: Cell -> Bool
containsTrash (Cell Trash _) = True
containsTrash _ = False

-- Returns the (row, column) 1-based index of the player element in the given environment;
-- if the player is not present, returns (-1, -1).
getPlayerLocation :: Environment -> Location
getPlayerLocation env =
  case findIndexMatrix env containsPlayer of
    Just location -> location
    Nothing -> (-1, -1)

-- Returns the number of rows for the given rectangular matrix.
getNumRows :: Matrix a -> Int
getNumRows mat = Data.Vector.length (getRow 1 mat)

-- Returns the number of columns for the given rectangular matrix.
getNumCols :: Matrix a -> Int
getNumCols mat = Data.Vector.length (getCol 1 mat)

-- Returns the first-occuring index of an element within the matrix that satisfies the given predicate;
-- Nothing if the element was not found.
findIndexMatrix :: (Eq a) => Matrix a -> (a -> Bool) -> Maybe Location
findIndexMatrix mat predicate =
  helper mat predicate (getNumRows mat) (getNumCols mat) 1 1
  where
    helper ::
         (Eq a)
      => Matrix a
      -> (a -> Bool)
      -> Int
      -> Int
      -> Int
      -> Int
      -> Maybe Location
    helper mat predicate numRows numCols row col
      | row > numRows = Nothing -- reached end of matrix without finding desired element
      | col > numCols = helper mat predicate numRows numCols (row + 1) 1 -- reached end of row
      | predicate (getElem row col mat) = Just (row, col)
      | otherwise = helper mat predicate numRows numCols row (col + 1)
