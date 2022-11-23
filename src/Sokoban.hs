-- Data types, world rules
module Sokoban
(
    GameObject(..),
    Background(..),
    Cell(..),
    Environment,
    Movement(..),
    sampleLevel,
    emptyCell,
    playerCell,
    wallCell,
    trashCell,
    isValidMove
)
    where
import Data.Matrix
import Data.List
import Data.Vector 
import Data.Ix (inRange)


-- An object that the player can interact with in the game
data GameObject = Player | Trash | Wall | Empty 
    deriving (Eq, Show)

-- The appearance of a grid cell
data Background = Trashcan | Stash | EmptyBG
    deriving (Eq, Show)

-- An individual cell within a level, consisting of both a GameObject and a Background
data Cell = MkCell {
    gameObject :: GameObject,
    background :: Background
}
    deriving (Eq, Show)

-- The representation of an individual level
type Environment = Matrix Cell 

-- level building
emptyCell :: Cell -- just for shorter code
emptyCell = MkCell {gameObject=Empty, background=EmptyBG}

playerCell :: Cell -- cell with player
playerCell = MkCell {gameObject=Player, background=EmptyBG}

wallCell :: Cell -- a wall
wallCell = MkCell {gameObject=Wall, background=EmptyBG}

trashcanCell :: Cell -- starting point of the trash
trashcanCell = MkCell {gameObject=Trash, background=Trashcan}

trashCell :: Cell -- a cell containing trash that is not the starting point
trashCell = MkCell {gameObject=Trash, background=EmptyBG}

emptystashCell :: Cell -- an empty stash
emptystashCell = MkCell {gameObject=Empty, background=Stash}

-- level definitions
sampleLevel :: Environment 
sampleLevel = fromLists [[emptyCell , emptyCell     , wallCell       , emptyCell]
                        ,[emptyCell , trashCell   , trashCell      , wallCell]
                        ,[emptyCell, trashCell , playerCell , wallCell]
                        ,[wallCell , wallCell     , wallCell       , emptyCell]
                        ]

-- The types of valid moves
data Movement = UpMv | RightMv | DownMv | LeftMv 
    deriving (Eq, Show)

-- Represents a (row, column) index in an Environment
type Location = (Int, Int)

-- Returns the Cell at the given location in the environment.
getCellAtLocation :: Location -> Environment -> Cell 
getCellAtLocation (row, col) env = getElem row col env 

-- Returns True if the given movement with respect to the given location is valid for the 
-- current environment; False otherwise.
isValidMove :: Movement -> Location -> Environment -> Bool 
isValidMove mv loc env = 
    (isWithinEnvironment loc env) && 
    (isWithinEnvironment destLoc env) && 
    (not (containsWall destCell)) && 
    (not (containsPlayer destCell)) && 
    ((containsEmptySpace destCell) || (containsTrash destCell && isValidPush mv destLoc env))
    where 
        destLoc = getLocationAfterMovement mv loc  
        destCell = getCellAtLocation destLoc env
        nextDestLoc = getLocationAfterMovement mv destLoc 
        nextDestCell = getCellAtLocation nextDestLoc

-- Returns True if the pushing behavior from the given location in the given direction of movement
-- is valid; False otherwise. Currently non-recursive.
isValidPush :: Movement -> Location -> Environment -> Bool 
isValidPush mv loc env = 
    (isWithinEnvironment loc env) && 
    (isWithinEnvironment destLoc env) && 
    (containsEmptySpace destCell)
    where 
        destLoc = getLocationAfterMovement mv loc  
        destCell = getCellAtLocation destLoc env

-- Returns True if the given location is within the environment; False otherwise.
isWithinEnvironment :: Location -> Environment -> Bool 
isWithinEnvironment (row, col) env = (inRange rowRange row) && (inRange colRange col)
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
containsWall (MkCell Wall _) = True 
containsWall _ = False

-- Returns True if the given Cell contains a Player; False otherwise.
containsPlayer :: Cell -> Bool 
containsPlayer (MkCell Player _) = True 
containsPlayer _ = False

-- Returns True if the given Cell contains an Empty space; False otherwise.
containsEmptySpace :: Cell -> Bool 
containsEmptySpace (MkCell Empty _) = True 
containsEmptySpace _ = False

-- Returns True if the given Cell contains a Trash; False otherwise.
containsTrash :: Cell -> Bool 
containsTrash (MkCell Trash _) = True 
containsTrash _ = False

-- Returns the (row, column) 1-based index of the player element in the given environment;
-- if the player is not present, returns (-1, -1).
getPlayerLocation :: Environment -> Location
getPlayerLocation env = case helper of 
    Just location -> location 
    Nothing -> (-1, -1)
    where 
        helper :: Maybe Location
        helper = findIndexMatrix env containsPlayer

-- Returns the number of rows for the given rectangular matrix.
getNumRows :: Matrix a -> Int 
getNumRows mat = Data.Vector.length (getRow 1 mat)

-- Returns the number of columns for the given rectangular matrix.
getNumCols :: Matrix a -> Int 
getNumCols mat = Data.Vector.length (getCol 1 mat)

-- Returns the first-occuring index of an element within the matrix that satisfies the given predicate;
-- Nothing if the element was not found.
findIndexMatrix :: (Eq a) => Matrix a -> (a -> Bool) -> Maybe Location
findIndexMatrix mat predicate = helper mat predicate (getNumRows mat) (getNumCols mat) 1 1
    where 
        helper :: (Eq a) => Matrix a -> (a -> Bool) -> Int -> Int -> Int -> Int -> Maybe Location
        helper mat predicate numRows numCols row col
            | row > numRows = Nothing -- reached end of matrix without finding desired element
            | col > numCols = helper mat predicate numRows numCols (row + 1) 1 -- reached end of row
            | predicate (getElem row col mat) = Just (row, col)
            | otherwise = helper mat predicate numRows numCols row (col + 1)
