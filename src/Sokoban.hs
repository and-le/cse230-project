-- The main file for the underlying logic of the game.
module Sokoban
  ( GameObject(..)
  , Background(..)
  , Cell(..)
  , Level(..)
  , Environment
  , Location
  , Movement(..)
  , MoveStatus(..)
  , move
  , emptyCell
  , playerCell
  , wallCell
  , trashCell
  , stashCell
  , isValidMove
  , getTrashCount
  , moveLevel
  , isLevelComplete
  , fromCells
  , getNumRows
  , getNumCols
  , countNumPlayers
  , countNumTrash
  , countNumWalls
  , countNumStashes
  , countNumEmpty
  ) where

import qualified Data.Ix (inRange)
import qualified Data.List (filter, length)
import Data.Matrix

-- An object that the player can interact with in the game
data GameObject
  = Player
  | Trash
  | Wall
  | Stash
  | Empty
  deriving (Eq, Show)

-- The appearance of a grid cell
data Background
  = Trashcan
  | StashBG
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

-- Level number and its environment
data Level =
  MkLevel
    { levelNum :: Int
    , name :: String
    , env :: Environment
    , trashCount :: Int
    , exit :: Bool
    , selectlvl :: Bool
    }

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
playerCell :: Cell -- cell with player
playerCell = Cell {gameObject = Player, background = EmptyBG}

trashCell :: Cell -- a cell containing trash that is not the starting point
trashCell = Cell {gameObject = Trash, background = EmptyBG}

wallCell :: Cell -- a wall
wallCell = Cell {gameObject = Wall, background = EmptyBG}

stashCell :: Cell -- an empty stash
stashCell = Cell {gameObject = Stash, background = StashBG}

emptyCell :: Cell -- just for shorter code
emptyCell = Cell {gameObject = Empty, background = EmptyBG}

trashcanCell :: Cell -- starting point of the trash
trashcanCell = Cell {gameObject = Trash, background = Trashcan}

-- Returns True if the given level is in a completed state; False otherwise
isLevelComplete :: Level -> Bool
isLevelComplete lvl = trashCount lvl == 0

-- Wrapper function around the regular `move` that will update game state after
-- the movement. Currently the count of trash left is updated.
moveLevel :: Movement -> Level -> Level
moveLevel mv lvl =
  MkLevel
    { name = name lvl
    , levelNum = levelNum lvl
    , env = newEnv
    , trashCount = newTrashCount
    , exit = False
    , selectlvl = False
    }
  where
    newEnv = move mv (env lvl)
    newTrashCount = getTrashCount newEnv

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
      -- The object in the from-location replaces the object in the to-location,
      -- unless the to-object is unpushable (i.e., a stash or wall).
      -- Trash should disappear if it is pushed into the stash
    fromElem' = Cell {gameObject = Empty, background = (background fromElem)}
    toElem' =
      Cell
        { gameObject =
            (if (containsStash toElem || containsWall toElem)
               then gameObject toElem
               else gameObject fromElem)
        , background = (background toElem)
        }
      -- If the object previously occupying the to-location is pushable, attempt to move it recursively.
    newEnv =
      if (gameObject toElem == Trash)
        then moveHelper mv toLoc env
        else env
    newEnv' = setElem toElem' toLoc (setElem fromElem' fromLoc newEnv)

-- Returns the count of Trash objects in the given Environment.
getTrashCount :: Environment -> Int
getTrashCount env =
  Data.List.length
    (Data.List.filter (\cell -> gameObject cell == Trash) (toList env))

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
    fromCell = getCellAtLocation loc env
    destLoc = getLocationAfterMovement mv loc
    destCell = getCellAtLocation destLoc env
    boundsCheck =
      (isWithinEnvironment loc env) &&
      (isWithinEnvironment destLoc env) &&
      (not (containsWall destCell)) &&
      (not (containsPlayer destCell)) &&
      (containsEmptySpace destCell ||
       (containsTrash destCell && (isValidMove mv destLoc env == ValidMov)) ||
       (containsTrash fromCell && containsStash destCell))

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

-- Returns True if the given Cell contains a Player; False otherwise.
containsPlayer :: Cell -> Bool
containsPlayer (Cell Player _) = True
containsPlayer _ = False

-- Returns True if the given Cell contains a Trash; False otherwise.
containsTrash :: Cell -> Bool
containsTrash (Cell Trash _) = True
containsTrash _ = False

-- Returns True if the given Cell contains a Wall, False otherwise.
containsWall :: Cell -> Bool
containsWall (Cell Wall _) = True
containsWall _ = False

-- Returns True if the given Cell contains a stash; False otherwise.
containsStash :: Cell -> Bool
containsStash (Cell Stash _) = True
containsStash _ = False

-- Returns True if the given Cell contains an Empty space; False otherwise.
containsEmptySpace :: Cell -> Bool
containsEmptySpace (Cell Empty _) = True
containsEmptySpace _ = False

-- Returns the number of cells in the Environment containing a player.
countNumPlayers :: Environment -> Int
countNumPlayers env = length (filter containsPlayer (toList env))

-- Returns the number of cells in the Environment containing a trash object.
countNumTrash :: Environment -> Int
countNumTrash env = length (filter containsTrash (toList env))

-- Returns the number of cells in the Environment containing a wall.
countNumWalls :: Environment -> Int
countNumWalls env = length (filter containsWall (toList env))

-- Returns the number of cells in the Environment containing a stash.
countNumStashes :: Environment -> Int
countNumStashes env = length (filter containsStash (toList env))

-- Returns the number of cells in the Environment containing an empty space.
countNumEmpty :: Environment -> Int
countNumEmpty env = length (filter containsEmptySpace (toList env))

-- Returns the (row, column) 1-based index of the player element in the given environment;
-- if the player is not present, returns (-1, -1).
getPlayerLocation :: Environment -> Location
getPlayerLocation env =
  case findIndexEnvironment env containsPlayer of
    Just location -> location
    Nothing -> (-1, -1)

-- Returns the number of rows for the given environment.
getNumRows :: Environment -> Int
getNumRows env = nrows env

-- Returns the number of columns for the given environment
getNumCols :: Environment -> Int
getNumCols env = ncols env

-- Returns the first-occuring index of an element within the Environment that satisfies the given predicate;
-- Nothing if the element was not found.
findIndexEnvironment :: Environment -> (Cell -> Bool) -> Maybe Location
findIndexEnvironment env predicate =
  helper env predicate (getNumRows env) (getNumCols env) 1 1
  where
    helper ::
         Environment
      -> (Cell -> Bool)
      -> Int
      -> Int
      -> Int
      -> Int
      -> Maybe Location
    helper env predicate numRows numCols row col
      | row > numRows = Nothing -- reached end of matrix without finding desired element
      | col > numCols = helper env predicate numRows numCols (row + 1) 1 -- reached end of row
      | predicate (getElem row col env) = Just (row, col)
      | otherwise = helper env predicate numRows numCols row (col + 1)

-- Creates an Environment from a grid of Cells 
fromCells :: [[Cell]] -> Environment
fromCells cells = fromLists cells
