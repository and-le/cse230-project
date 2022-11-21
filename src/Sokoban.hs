-- Data types, world rules
module Sokoban
(
    GameObject(..),
    Background(..),
    Cell(..),
    Environment,
    sampleLevel
)
    where
import Data.Matrix
import Data.List
import Data.Vector 


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

emptystashCell :: Cell -- an empty stash
emptystashCell = MkCell {gameObject=Empty, background=Stash}

-- level definitions
sampleLevel :: Environment 
sampleLevel = fromLists [[emptyCell , emptyCell     , wallCell       , emptyCell]
                        ,[emptyCell , emptyCell   , playerCell      , wallCell]
                        ,[emptyCell, trashcanCell , emptystashCell , wallCell]
                        ,[wallCell , wallCell     , wallCell       , emptyCell]
                        ]

data PlayerMovement = Up | Right | Down | Left 
    deriving (Eq, Show)

-- Returns True if the given movement is valid given the current environment; False otherwise.
isValidMove :: PlayerMovement -> Environment -> Bool 
isValidMove pm env = isValidMoveCached pm env (getPlayerLocation environment)

-- Same as `isValidMove` but uses a cached player location for efficiency.
-- Ideally we will be storing the player's current location throughout the game so
-- we won't need to query for it upon each move.
isValidMoveCached :: PlayerMovement -> Environment -> (Int, Int) -> Bool 
isValidMoveCached pm env (row, col) = error "TODO"

-- Add the rest of the logic here


-- Returns the (row, column) 1-based index of the player element in the given environment;
-- if the player is not present, returns (-1, -1).
getPlayerLocation :: Environment -> (Int, Int)
getPlayerLocation env = case helper of 
    Just location -> location 
    Nothing -> (-1, -1)
    where 
        helper :: Maybe (Int, Int)
        helper = findIndexMatrix env containsPlayer

-- Returns True if the given Cell contains the Player, False otherwise.
containsPlayer :: Cell -> Bool 
containsPlayer (MkCell Player _) = True 
containsPlayer _ = False

-- Returns the number of rows for the given rectangular matrix.
getNumRows :: Matrix a -> Int 
getNumRows mat = Data.Vector.length (getRow 1 mat)

-- Returns the number of columns for the given rectangular matrix.
getNumCols :: Matrix a -> Int 
getNumCols mat = Data.Vector.length (getCol 1 mat)

-- Returns the first-occuring index of an element within the matrix that satisfies the given predicate;
-- Nothing if the element was not found.
findIndexMatrix :: (Eq a) => Matrix a -> (a -> Bool) -> Maybe (Int, Int)
findIndexMatrix mat predicate = helper mat predicate (getNumRows mat) (getNumCols mat) 1 1
    where 
    helper :: (Eq a) => Matrix a -> (a -> Bool) -> Int -> Int -> Int -> Int -> Maybe (Int, Int)
    helper mat predicate numRows numCols row col
        | row > numRows = Nothing -- reached end of matrix without finding desired element
        | col > numCols = helper mat predicate numRows numCols (row + 1) 1 -- reached end of row
        | predicate (getElem row col mat) = Just (row, col)
        | otherwise = helper mat predicate numRows numCols row (col + 1)
