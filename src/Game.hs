-- The main file for the interface between the underlying game and Brick.
module Game where 

import Data.Matrix 
import Data.Text (pack)

import Brick (
               App(..), BrickEvent(..), EventM, Widget, Next,
               (<+>), str, withBorderStyle, emptyWidget, padLeftRight,
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


handleEvent :: Level -> BrickEvent Name e -> EventM Name (Next Level)
-- (Esc, q) key to quit
handleEvent lvl (VtyEvent (V.EvKey V.KEsc [])) = halt MkLevel {levelNum = -1, env = env lvl, trashCount = -1, exit = True, selectlvl = False}
handleEvent lvl (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt MkLevel {levelNum = -1, env = env lvl, trashCount = -1, exit = True, selectlvl = False}
handleEvent lvl (VtyEvent (V.EvKey (V.KChar 'n') [])) = halt lvl
-- Reset Level
handleEvent lvl (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ resetLevel lvl

-- Go back to level select
handleEvent lvl (VtyEvent (V.EvKey V.KBS [])) = halt MkLevel {levelNum = -1, env = env lvl, trashCount = -1, exit = False, selectlvl=True}

-- Player Movement. We first perform the movement and then check whether the level has been completed. If so,
-- we halt.
handleEvent lvl (VtyEvent (V.EvKey V.KUp [])) = if (isLevelComplete lvl') then halt lvl' else continue lvl' 
  where
    lvl' = moveLevel UpMv lvl 
handleEvent lvl (VtyEvent (V.EvKey V.KRight [])) = if (isLevelComplete lvl') then halt lvl' else continue lvl' 
  where
    lvl' = moveLevel RightMv lvl 
handleEvent lvl (VtyEvent (V.EvKey V.KDown [])) = if (isLevelComplete lvl') then halt lvl' else continue lvl' 
  where
    lvl' = moveLevel DownMv lvl 
handleEvent lvl (VtyEvent (V.EvKey V.KLeft [])) = if (isLevelComplete lvl') then halt lvl' else continue lvl' 
  where
    lvl' = moveLevel LeftMv lvl 

-- Unrecognized keys
handleEvent lvl _                              = continue lvl

-- does nothing right now
attributes :: AttrMap
attributes = attrMap V.defAttr [(attrName "player", fg V.cyan)]

drawGrid :: Level -> [Widget Name]
drawGrid lvl = [center $
    B.borderWithLabel (str "Raccoon Rush") $
    renderTable (setDefaultRowAlignment AlignMiddle $
    setDefaultColAlignment AlignCenter $
    convertMap2Table (env lvl))]

cell2string :: GameObject -> String 
cell2string Player = "𓃠"
cell2string Empty = " "
cell2string Trash = "◌"
cell2string Wall = "▩"

convertMap2Table :: Environment -> Table n
convertMap2Table m = table (map (map (\x -> padLeftRight 1 (txt (pack (cell2string x))))) $ (map (map gameObject) (toLists m)))

-- Handles the exiting of a level.
-- If `exit` is True, calls `appExit`.
-- Otherwise, if the given level is not the last level in the game, returns the next level.
-- If the given level is the final level, calls `appComplete`.
handleLevelExit :: Level -> IO Level
handleLevelExit lvl = 
  if exit lvl
    then 
      defaultMain appExit lvl 
  else 
    if selectlvl lvl
      then
        do
          level <- levelSelect
          return (MkLevel {levelNum = (levelNum level), exit=False, selectlvl=True})
      else
        if levelNum lvl < max_level 
          then 
            do 
              return (resetLevel MkLevel {levelNum = (levelNum lvl + 1)})
          else 
            do 
              defaultMain appComplete (MkLevel {exit = True}) 


-- UI for exiting Raccoon Rush
exitUI :: Widget ()
exitUI =
    center
    $ withBorderStyle unicode
    $ B.borderWithLabel (str "Raccoon Rush")
    $ str "Thanks for trying Raccoon Rush! Goodbye!"


-- UI for intermission between levels
intermissionUI :: Widget ()
intermissionUI =
    center
    $ withBorderStyle unicode
    $ B.borderWithLabel (str "Raccoon Rush")
    $ str "Level Complete! Press SPACE to continue."

-- UI for completing Raccoon Rush
completeUI :: Widget ()
completeUI =
    center
    $ withBorderStyle unicode
    $ B.borderWithLabel (str "Raccoon Rush")
    $ str "Congratulations! You completed Raccoon Rush!"

handleAppExit :: Level -> BrickEvent () e -> EventM Name (Next Level)
handleAppExit lvl (VtyEvent (V.EvKey V.KEsc [])) = halt lvl
handleAppExit lvl _                              = halt lvl

appExit :: App Level e Name
appExit = App
  { appDraw         = const [exitUI]
  , appHandleEvent  = handleAppExit
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

-- Event Handling for intermission screen
handleAppIntermission :: Level -> BrickEvent () e -> EventM Name (Next Level)
handleAppIntermission lvl (VtyEvent (V.EvKey V.KEsc [])) = halt lvl
handleAppIntermission lvl (VtyEvent (V.EvKey (V.KChar ' ') [])) = halt lvl
handleAppIntermission lvl _                              = continue lvl

-- UI for between-stage intermission
appIntermission :: App Level e Name
appIntermission = App
  { appDraw         = const [intermissionUI]
  , appHandleEvent  = handleAppIntermission
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

handleAppComplete :: Level -> BrickEvent () e -> EventM Name (Next Level)
handleAppComplete lvl (VtyEvent (V.EvKey V.KEsc [])) = halt lvl
handleAppComplete lvl _ = halt lvl

appComplete :: App Level e Name
appComplete = App
  { appDraw         = const [completeUI]
  , appHandleEvent  = handleAppComplete
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

-- Starts the entire game
main :: IO ()
main = do 
  -- Show level select screen
  initialLevel <- levelSelect 
  mainLoop initialLevel   

-- Main loop that handles exiting a level and what to do afterward
mainLoop :: Level -> IO () 
mainLoop lvl = do 
  if (exit lvl)
    then 
      do
        -- Handle exiting from a level
        _ <- handleLevelExit lvl
        return ()
  else 
    do 
      -- Transition to a level
      exitedLevel <- defaultMain app lvl
      -- Handle exiting from a level
      nextLevel <- handleLevelExit exitedLevel 
      -- Check has exited
      if (exit nextLevel)
        then 
          return ()
      else
        if (selectlvl nextLevel)
          then
            mainLoop (resetLevel nextLevel)
          else
            do 
              -- Show an intermission screen
              _ <- defaultMain appIntermission nextLevel
              -- Start the next level
              mainLoop nextLevel
