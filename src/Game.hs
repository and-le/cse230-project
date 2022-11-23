-- UI, event handlers 
{-# LANGUAGE OverloadedStrings #-}

module Game where 

import Data.Matrix 
import Data.Text (pack)

import Brick (
               App(..), BrickEvent(..), EventM, Widget, Next,
               (<+>), str, withBorderStyle, emptyWidget,
               neverShowCursor, vBox, defaultMain, txt, continue, halt
             )
import Brick.AttrMap
import Brick.Util (fg)
import Brick.Widgets.Table
import Brick.Widgets.Center (center)
import Brick.Widgets.Border.Style (unicode)

import Graphics.Vty as V

import Sokoban

type Name = ()

app :: App Environment e Name
app = App
  { appDraw         = drawGrid
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const attributes
  }

-- Esc key to quit
handleEvent :: Environment -> BrickEvent Name e -> EventM Name (Next Environment)
handleEvent env (VtyEvent (V.EvKey V.KEsc [])) = halt env
handleEvent env (VtyEvent (V.EvKey V.KUp [])) = continue $ move UpMv env
handleEvent env (VtyEvent (V.EvKey V.KRight [])) = continue $ move RightMv env
handleEvent env (VtyEvent (V.EvKey V.KDown [])) = continue $ move DownMv env
handleEvent env (VtyEvent (V.EvKey V.KLeft [])) = continue $ move LeftMv env
handleEvent env _                              = continue env

-- does nothing right now
attributes :: AttrMap
attributes = attrMap V.defAttr [(attrName "player", fg V.cyan)]

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



main :: IO ()
main = do 
    s <- defaultMain app sampleLevel
    putStrLn "DONE"

-- Event Handlers