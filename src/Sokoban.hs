-- Data types, world rules
module Sokoban
(
    GameObject(..),
    Background(..),
    Cell(..),
    Environment
)
    where
import Data.Matrix


-- An object that the player can interact with in the game
data GameObject = Player | Trash | Wall | Empty 
    deriving (Eq, Show)

-- The appearance of a grid cell
data Background = Trashcan | Stash | EmptyCell
    deriving (Eq, Show)

-- An individual cell within a level, consisting of both a GameObject and a Background
data Cell = MkCell {
    gameObject :: GameObject,
    background :: Background
}
    deriving (Eq, Show)

-- The representation of an individual level
type Environment = Matrix Cell 

-- Add game logic functions here 