-- UI, event handlers 
-- {-# LANGUAGE OverloadedStrings #-}

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
import qualified Brick.Widgets.Border as B

import Sokoban
import LevelSelect

type Name = ()

app :: App Level e Name
app = App
  { appDraw         = drawGrid
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const attributes
  }

-- Apply a function to a level's environment
applyMoveLevel ::  (Environment -> Environment) -> Level -> Level
applyMoveLevel f lvl = MkLevel {levelNum = levelNum lvl, env = f (env lvl), exit = False}

handleEvent :: Level -> BrickEvent Name e -> EventM Name (Next Level)
-- (Esc, q) key to quit
handleEvent lvl (VtyEvent (V.EvKey V.KEsc [])) = halt MkLevel {levelNum = -1, env = env lvl, exit = True}
handleEvent lvl (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt MkLevel {levelNum = -1, env = env lvl, exit = True}
handleEvent lvl (VtyEvent (V.EvKey (V.KChar 'n') [])) = halt lvl
-- Reset Level
handleEvent lvl (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ resetLevel lvl
-- Player Movement
handleEvent lvl (VtyEvent (V.EvKey V.KUp [])) = continue $ applyMoveLevel (move UpMv) lvl
handleEvent lvl (VtyEvent (V.EvKey V.KRight [])) = continue $ applyMoveLevel (move RightMv) lvl
handleEvent lvl (VtyEvent (V.EvKey V.KDown [])) = continue $ applyMoveLevel (move DownMv) lvl
handleEvent lvl (VtyEvent (V.EvKey V.KLeft [])) = continue $ applyMoveLevel (move LeftMv) lvl
handleEvent lvl _                              = continue lvl

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


drawGrid :: Level -> [Widget Name]
drawGrid lvl = [renderTable (setDefaultRowAlignment AlignMiddle $
    setDefaultColAlignment AlignCenter $
    convertMap2Table (env lvl))]


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

nextLevel :: Level -> IO Level
nextLevel lvl = 
  if exit lvl == True
    then
      defaultMain appExit lvl
    else  
      if levelNum lvl < max_level
        then
          do 
            let nextLvl = resetLevel MkLevel {levelNum=(levelNum lvl + 1), env=(env lvl), exit=False}
            lvl <- defaultMain app nextLvl
            nextLevel lvl
          else
            do
              defaultMain appComplete lvl

-- UI for exiting Raccoon Rush
exitUI :: Widget ()
exitUI =
    center
    $ withBorderStyle unicode
    $ B.borderWithLabel (str "Raccoon Rush")
    $ str "Thanks for trying Raccoon Rush! Goodbye!"

-- UI for completing Raccoon Rush
completeUI :: Widget ()
completeUI =
    center
    $ withBorderStyle unicode
    $ B.borderWithLabel (str "Raccoon Rush")
    $ str "Congratulations! You completed Raccoon Rush!"

appExit :: App Level e Name
appExit = App
  { appDraw         = const [exitUI]
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

appComplete :: App Level e Name
appComplete = App
  { appDraw         = const [completeUI]
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

main :: IO ()
main = do 
    level <- levelSelect
    lvl <- defaultMain app level
    nextLevel lvl

    putStrLn "DONE"

-- Event Handlers