-- UI, event handlers 

module Game where 

import Data.Matrix 

import Brick ( App(..), BrickEvent(..), EventM, Widget, (<+>), str, withBorderStyle, emptyWidget)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border.Style (unicode)

import Sokoban

type Name = ()

app :: App Environment e ()
app = App
  { appDraw         = drawGrid Environment
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = e
  , appStartEvent   = return ()
  , appAttrMap      = ()
  }

drawCol rowList =
    vBox rowList
    
drawRow :: -- TODO
drawRow = 
    foldr (<+>) emptyWidget row 
    where
        row = TODO

drawGrid :: Environment -> Widget Name
drawGrid grid = 
    withBorderStyle unicode $
    borderWithLabel (str "Raccoon Rush") $
    center (str "DATA_MATRIX HERE")

drawCell :: Cell -> Widget Name
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
sampleLevel = fromLists [[MkCell Empty Empty, MkCell Empty Empty, MkCell Wall Empty]
                        ,[MkCell Empty Empty, MkCell Player Empty, MkCell Empty Empty]
                        ,[MkCell Empty Empty, MkCell Trash Empty, MkCell Empty Stash]
]


main :: IO ()
main = defaultMain app initalState

-- Event Handlers