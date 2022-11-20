-- UI, event handlers 

module Game where 

import Data.Matrix 

import Brick ( App(..), BrickEvent(..), EventM, Widget, (<+>), str, withBorderStyle, emptyWidget, neverShowCursor, vBox, defaultMain)
import Brick.AttrMap
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border.Style (unicode)

import Sokoban

type Name = ()

app :: App Environment e ()
app = App
  { appDraw         = drawGrid
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = attributes
  }

handleEvent :: BrickEvent Name e -> EventM Name Environment ()
handleEvent = error "TODO"

attributes :: Environment -> AttrMap
attributes = error "TODO"

drawCol rowList =
    vBox rowList
    
-- drawRow :: -- TODO
-- drawRow = 
--     foldr (<+>) emptyWidget row 
--     where
--         row = TODO

drawGrid :: Environment -> [Widget Name]
drawGrid grid = 
    [withBorderStyle unicode $
    borderWithLabel (str "Raccoon Rush") $
    center (str "DATA_MATRIX HERE")]

drawCell :: GameObject -> Widget Name
drawCell Player =
    withBorderStyle unicode $
    border $
    center (str "Player")
drawCell Empty =
    withBorderStyle unicode $
    border $
    center (str "")
drawCell Trash =
    withBorderStyle unicode $
    border $
    center (str "Trash")
drawCell Wall =
    withBorderStyle unicode $
    border $
    center (str "Wall")


sampleLevel :: Environment
sampleLevel = fromLists [[MkCell {gameObject=Empty, background=EmptyCell}, MkCell {gameObject=Empty, background=EmptyCell}, MkCell {gameObject=Wall, background=EmptyCell}]
                        ,[MkCell {gameObject=Empty, background=EmptyCell}, MkCell {gameObject=Player, background=EmptyCell}, MkCell {gameObject=Empty, background=EmptyCell}]
                        ,[MkCell {gameObject=Empty, background=EmptyCell}, MkCell {gameObject=Trash, background=EmptyCell}, MkCell {gameObject=Empty, background=Stash}]
                        ]


main :: IO ()
main = do 
    s <- defaultMain app sampleLevel
    putStrLn "DONE"

-- Event Handlers