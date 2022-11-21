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
sampleLevel = fromLists [[wallCell , wallCell     , wallCell       , wallCell]
                        ,[wallCell , playerCell   , emptyCell      , wallCell]
                        ,[wallCell , trashcanCell , emptystashCell , wallCell]
                        ,[wallCell , wallCell     , wallCell       , wallCell]
                        ]

-- Add game logic functions here