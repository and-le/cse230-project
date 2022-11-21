-- UI, event handlers 
{-# LANGUAGE OverloadedStrings #-}

module Game where 

import Data.Matrix 
import Data.Text (pack)

import Brick (
               App(..), BrickEvent(..), EventM, Widget, Next,
               (<+>), str, withBorderStyle, emptyWidget,
               neverShowCursor, vBox, defaultMain,txt
             )
import Brick.AttrMap
import Brick.Widgets.Table
import Brick.Widgets.Center (center)
import Brick.Widgets.Border.Style (unicode)

import Sokoban

type Name = ()

app :: App Environment e Name
app = App
  { appDraw         = drawGrid
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = attributes
  }

handleEvent :: Environment -> BrickEvent Name e -> EventM Name (Next Environment)
handleEvent = error "TODO"

attributes :: Environment -> AttrMap
attributes = error "TODO"

-- drawCol rowList =
--     vBox rowList
    
-- drawRow :: -- TODO
-- drawRow = 
--     foldr (<+>) emptyWidget row 
--     where
--         row = TODO




drawGrid :: Environment -> [Widget Name]
drawGrid grid = [renderTable (setDefaultRowAlignment AlignMiddle $
    setDefaultColAlignment AlignCenter $
    convertMap2Table grid)]


cell2string Player = "P"
cell2string Empty = " "
cell2string Trash = "T"
cell2string Wall = "W"

convertMap2Table :: Environment -> Table n
convertMap2Table m = table (map (map (\x -> txt (pack (cell2string x)))) $ (map (map gameObject) (toLists m)))

-- test table
-- convertMap2Table :: Matrix String -> Table n
-- convertMap2Table m = table (map (map (\x -> txt (pack x))) $ (toLists m))


-- drawGrid grid = 
--     withBorderStyle unicode $
--     borderWithLabel (str "Raccoon Rush") $
--     center (str "DATA_MATRIX HERE")

-- drawCell :: Cell -> Widget Name
-- drawCell Player =
--     withBorderStyle unicode $
--     border $
--     center (str "Player")
-- drawCell Empty =
--     withBorderStyle unicode $
--     border $
--     center (str "")
-- drawCell Trash =
--     withBorderStyle unicode $
--     border $
--     center (str "Trash")
-- drawCell Wall =
--     withBorderStyle unicode $
--     border $
--     center (str "Wall")


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