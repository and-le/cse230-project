-- Level Select and various Levels
module LevelSelect
(
    levelSelect,
    resetLevel,
    max_level,
)
    where
import Sokoban
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import qualified Data.Matrix

type Name = ()

-- datatype to store level select screen state
data Selector = MkSelector {
  highlighted :: Int
}

app :: App Selector e Name
app = App
  { appDraw         = drawUI
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const lvlAttrMap
  , appChooseCursor = neverShowCursor
  }

-- opening screen state
default_selector :: Selector
default_selector = MkSelector {highlighted=0}

-- Level Select List for Display
levels = [
    ("Level 0 - " ++ name level_0, 0), 
    ("Level 1 - " ++ name level_1, 1), 
    ("Level 2 - " ++ name level_2, 2),
    ("Level 3 - " ++ name level_3, 3),
    ("Level 4 - " ++ name level_4, 4),
    ("Level 5 - " ++ name level_5, 5),
    ("Level 6 - " ++ name level_6, 6),
    ("Level 7 - " ++ name level_7, 7),
    ("Level 8 - " ++ name level_8, 8),
    ("Level 9 - " ++ name level_9, 9)
  ]
concat_bullet = ("-" ++)

levels_list i = map drawOptions levels
  where
    drawOptions (s,l) = if l==i then
                            withAttr (attrName "selected") $ str ("->" ++ s) 
                        else
                            str (concat_bullet s)


-- Level Select UI
drawUI :: Selector -> [Widget ()]
drawUI env =
    [ vBox
      [ C.center $ vBox
      [C.hCenter $ padBottom (Pad 3) $ str "⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⢉⣴⣶⣤⠀⣿⣿\n⡿⢛⣉⣉⣉⣉⡛⠻⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⠛⣡⣴⠏⠁⠀⡚⠀⢸⣿\n⠁⣿⠉⢀⡉⠉⢿⣷⣄⠈⠉⠁⣠⣀⣤⡀⣤⣤⣀⣴⠂⢺⠧⠀⠀⠀⠀⠀⣾⣿\n⠀⢷⣀⠀⠀⠀⠈⠙⣷⠀⠀⣿⣿⣿⣿⣿⢿⣿⡟⠃⠠⢄⣀⢀⡴⠀⣠⣾⣿⣿\n⣧⠈⠣⣦⠀⠀⠀⠀⠉⠸⠛⠃⠉⠋⠹⠀⠃⠁⠀⠀⠀⠺⣟⣷⣦⣄⠙⠿⣿⣿\n⣿⣇⢈⢿⣷⣾⣦⡤⢴⣿⣶⡀⢠⣔⣤⣤⡀⠀⠲⣾⣿⣶⣿⣿⣿⣿⣷⣄⠈⢻\n⣿⠏⣠⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⠿⡻⣿⠀⠘⠿⡟⠡⠄⢉⡼⢿⣿⣿⣦⠈\n⡏⣨⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⡁⠐⠃⠈⠀⠐⠀⠀⣀⢠⡤⣾⠃⠀⠉⠛⠛⢠\n⠁⣿⣿⣿⣿⣿⣿⣿⠏⠁⠘⠿⣶⣤⠴⠶⢟⣰⣶⣾⣿⣷⣤⣀⠀⠀⠀⠀⣠⣾\n⡀⢿⣿⣿⣿⡿⠋⠁⠀⠀⠀⠀⠉⠁⢠⣴⣿⣿⣿⣿⢿⣟⠿⣿⡆⠀⢀⣾⣿⣿\n⣷⡄⠛⠟⠃⠀⠀⠀⠀⠀⠀⢠⣤⣶⣿⣿⣿⣿⣿⡃⠈⠁⠀⠘⣡⣾⣿⣿⣿⣿\n⣿⣿⣶⣤⣤⣤⣄⣀⣀⣀⣀⣀⣈⣛⣉⣉⡙⠋⠋⠁⠀⢀⣠⣾⣿⣿⣿⣿⣿⣿"
        ,C.hCenter $ withBorderStyle BS.unicodeBold
          $ B.borderWithLabel (str "Raccoon Rush")
          $ vBox
          [ padLeftRight 3 $ padTopBottom 1 $ foldl (<=>) (str "Level Select:") (levels_list (highlighted env))]]
        , str "Use arrow and enter keys, or type the number to select a level, esc/q to exit"
      ]
    ]

-- raccoon copypasta from https://www.twitchquotes.com/copypastas/3565

-- Level Select Attr map
lvlAttrMap :: AttrMap
lvlAttrMap = attrMap V.defAttr
  [ (attrName "selected", fg V.blue `V.withStyle` V.bold) ]

---- Level Definitions
max_level :: Int
max_level = 9


-- Empty Level
empty_lvl :: Level
empty_lvl = MkLevel {name="Empty", levelNum=(-1), env=empty_lvl_env, trashCount=(-1), exit=False, selectlvl=False}
empty_lvl_env :: Environment
empty_lvl_env = Data.Matrix.fromLists [[playerCell]]

-- Level 0
level_0 :: Level
level_0 = MkLevel {name="Trash to Stash", levelNum=(0), env=level_0_env, trashCount=getTrashCount level_0_env, exit=False, selectlvl=False}
level_0_env :: Environment

level_0_env = Data.Matrix.fromLists [
    [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [playerCell, trashcanCell, emptyCell, emptyCell, stashCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]
  ]

-- Level 1
level_1 :: Level
level_1 = MkLevel {name="Geisel", levelNum=(1), env=level_1_env, trashCount=getTrashCount level_1_env, exit=False, selectlvl=False}
level_1_env :: Environment
level_1_env = Data.Matrix.fromLists [
    [emptyCell, emptyCell, emptyCell, wallCell, wallCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, wallCell, wallCell, wallCell, wallCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, wallCell, wallCell, emptyCell, emptyCell, emptyCell],
    [wallCell, wallCell, wallCell, wallCell, wallCell, wallCell, wallCell, wallCell],
    [emptyCell, emptyCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell],
    [playerCell, trashcanCell, emptyCell, emptyCell, wallCell, emptyCell, stashCell, emptyCell],
    [emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]
  ]

-- Level 2
level_2 :: Level
level_2 = MkLevel {name="The Labs", levelNum=(2), env=level_2_env, trashCount=getTrashCount level_2_env, exit=False, selectlvl=False}
level_2_env :: Environment
level_2_env = Data.Matrix.fromLists [
    [emptyCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, trashcanCell, emptyCell, wallCell, emptyCell, trashcanCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, trashcanCell, emptyCell],
    [wallCell, wallCell, emptyCell, wallCell, wallCell, emptyCell, wallCell, wallCell],
    [emptyCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [playerCell, emptyCell, wallCell, emptyCell, emptyCell, wallCell, wallCell, wallCell],
    [emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, trashcanCell, stashCell]
  ]

-- Level 3
level_3 :: Level
level_3 = MkLevel {name="The Dungeon", levelNum=(3), env=level_3_env, trashCount=getTrashCount level_3_env, exit=False, selectlvl=False}
level_3_env :: Environment
level_3_env = Data.Matrix.fromLists [
    [emptyCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, stashCell],
    [emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell, wallCell, wallCell, wallCell],
    [wallCell, emptyCell, emptyCell, trashcanCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, trashcanCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [playerCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]
  ]

-- Level 4
level_4 :: Level
level_4 = MkLevel {name="Choo-choo!", levelNum=(4), env=level_4_env, trashCount=getTrashCount level_4_env, exit=False, selectlvl=False}
level_4_env :: Environment
level_4_env = Data.Matrix.fromLists [
    [playerCell, emptyCell, trashcanCell, emptyCell, trashcanCell, emptyCell, trashcanCell, emptyCell, trashcanCell, emptyCell, emptyCell]
  ]

-- Level 5
level_5 :: Level
level_5 = MkLevel {name="I like trolleys", levelNum=(5), env=level_5_env, trashCount=getTrashCount level_5_env, exit=False, selectlvl=False}
level_5_env :: Environment
level_5_env = Data.Matrix.fromLists [
    [stashCell, wallCell, emptyCell, wallCell, emptyCell, emptyCell, trashcanCell, stashCell],
    [trashcanCell, wallCell, emptyCell, wallCell, emptyCell, trashcanCell, wallCell, wallCell],
    [emptyCell, trashcanCell, emptyCell, wallCell, emptyCell, trashcanCell, emptyCell, emptyCell],
    [emptyCell, trashcanCell, emptyCell, wallCell, emptyCell, trashcanCell, emptyCell, emptyCell],
    [emptyCell, trashcanCell, emptyCell, emptyCell, emptyCell, trashcanCell, emptyCell, emptyCell],
    [emptyCell, trashcanCell, emptyCell, wallCell, emptyCell, trashcanCell, emptyCell, emptyCell],
    [emptyCell, trashcanCell, emptyCell, wallCell, emptyCell, emptyCell, playerCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell]
  ]

-- Level 6
level_6 :: Level
level_6 = MkLevel {name="So close, yet so far", levelNum=(6), env=level_6_env, trashCount=getTrashCount level_6_env, exit=False, selectlvl=False}
level_6_env :: Environment
level_6_env = Data.Matrix.fromLists [
    [emptyCell, emptyCell, emptyCell, wallCell, wallCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell, wallCell, emptyCell],
    [emptyCell, wallCell, wallCell, wallCell, wallCell, wallCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, emptyCell, wallCell, wallCell, emptyCell, emptyCell],
    [wallCell, wallCell, wallCell, emptyCell, emptyCell, emptyCell, stashCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, wallCell, wallCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, trashcanCell, emptyCell, emptyCell, emptyCell, emptyCell, wallCell, emptyCell],
    [wallCell, playerCell, emptyCell, wallCell, emptyCell, emptyCell, wallCell, emptyCell]
  ]

-- Level 7
level_7 :: Level
level_7 = MkLevel {name="Lining up", levelNum=(7), env=level_7_env, trashCount=getTrashCount level_7_env, exit=False, selectlvl=False}
level_7_env :: Environment
level_7_env = Data.Matrix.fromLists [
    [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, wallCell],
    [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, wallCell, emptyCell, wallCell],
    [emptyCell, emptyCell, trashcanCell, trashcanCell, trashcanCell, trashcanCell, emptyCell, wallCell],
    [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, wallCell, emptyCell, wallCell],
    [emptyCell, emptyCell, trashcanCell, trashcanCell, trashcanCell, trashcanCell, emptyCell, wallCell],
    [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, wallCell, emptyCell, wallCell],
    [emptyCell, playerCell, emptyCell, emptyCell, emptyCell, wallCell, stashCell, wallCell],
    [emptyCell, stashCell, emptyCell, emptyCell, emptyCell, wallCell, wallCell, wallCell]
  ]

-- Level 8
level_8 :: Level
level_8 = MkLevel {name="Franklin-Antonio Hall", levelNum=(8), env=level_8_env, trashCount=getTrashCount level_8_env, exit=False, selectlvl=False}
level_8_env :: Environment
level_8_env = Data.Matrix.fromLists [
    [emptyCell, emptyCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, trashcanCell, trashcanCell, trashcanCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, trashcanCell, wallCell, trashcanCell, emptyCell, trashcanCell, trashcanCell, trashcanCell, emptyCell],
    [emptyCell, trashcanCell, trashcanCell, trashcanCell, emptyCell, trashcanCell, wallCell, trashcanCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, trashcanCell, trashcanCell, trashcanCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, wallCell, wallCell, wallCell, emptyCell, emptyCell, emptyCell],
    [wallCell, emptyCell, wallCell, wallCell, stashCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, wallCell, wallCell, wallCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, emptyCell, emptyCell, wallCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, trashcanCell, trashcanCell, trashcanCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, trashcanCell, wallCell, trashcanCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, trashcanCell, trashcanCell, trashcanCell, emptyCell, emptyCell, emptyCell, emptyCell],
    [emptyCell, emptyCell, playerCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]
  ]

-- Level 9
level_9 :: Level
level_9 = MkLevel {name="Pepper Canyon", levelNum=(9), env=level_9_env, trashCount=getTrashCount level_9_env, exit=False, selectlvl=False}
level_9_env :: Environment
level_9_env = Data.Matrix.fromLists [
    [emptyCell, emptyCell, trashcanCell, trashcanCell, trashcanCell, trashcanCell, stashCell, emptyCell],
    [emptyCell, emptyCell, wallCell, wallCell, wallCell, wallCell, wallCell, emptyCell],
    [wallCell, trashcanCell, emptyCell, emptyCell, emptyCell, trashcanCell, emptyCell, wallCell],
    [wallCell, emptyCell, emptyCell, emptyCell, emptyCell, trashcanCell, emptyCell, wallCell],
    [wallCell, trashcanCell, emptyCell, trashcanCell, trashcanCell, emptyCell, emptyCell, wallCell],
    [wallCell, emptyCell, trashcanCell, playerCell, trashcanCell, emptyCell, emptyCell, wallCell],
    [wallCell, emptyCell, emptyCell, trashcanCell, emptyCell, emptyCell, emptyCell, wallCell],
    [wallCell, wallCell, stashCell, wallCell, wallCell, stashCell, wallCell, wallCell]
  ]

-- Event Handling for Level Select Screen
handleEvent :: Selector -> BrickEvent () e -> EventM Name (Next Selector)
-- Exiting Level Select
handleEvent env (VtyEvent (V.EvKey V.KEsc [])) = halt $ MkSelector {highlighted = -1}
handleEvent env (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt $ MkSelector {highlighted = -1}
-- Level Select Event
handleEvent env (VtyEvent (V.EvKey (V.KChar d) [])) =
  if d `elem` ['0' .. ((show max_level) !! 0)] then
    do
        case read [d] of
            0 -> halt $ MkSelector {highlighted=0}
            1 -> halt $ MkSelector {highlighted=1}
            2 -> halt $ MkSelector {highlighted=2}
            3 -> halt $ MkSelector {highlighted=3}
            4 -> halt $ MkSelector {highlighted=4}
            5 -> halt $ MkSelector {highlighted=5}
            6 -> halt $ MkSelector {highlighted=6}
            7 -> halt $ MkSelector {highlighted=7}
            8 -> halt $ MkSelector {highlighted=8}
            9 -> halt $ MkSelector {highlighted=9}
            _ -> continue env
  else continue env
handleEvent env (VtyEvent (V.EvKey V.KUp []))  = continue $ cursorUp
  where
    l = highlighted env
    cursorUp = if l > 0 then
                 MkSelector {highlighted=l-1}
               else
                 MkSelector {highlighted=l}
handleEvent env (VtyEvent (V.EvKey V.KDown []))= continue $ cursorDown
  where
    l = highlighted env
    cursorDown = if (l < (length levels)-1) then
                   MkSelector {highlighted=l+1}
                 else
                   MkSelector {highlighted=l}
handleEvent env (VtyEvent (V.EvKey V.KEnter []))= halt env
handleEvent env _                              = continue env

-- Level Select Screen
levelSelect :: IO Level
levelSelect = do 
                l <- defaultMain app default_selector
                return (intToLvl (highlighted l))

-- go from an Int to a level
intToLvl :: Int -> Level
intToLvl i =
    case i of
        0 -> level_0
        1 -> level_1
        2 -> level_2
        3 -> level_3
        4 -> level_4
        5 -> level_5
        6 -> level_6
        7 -> level_7
        8 -> level_8
        9 -> level_9
        _ -> MkLevel {name="", levelNum = -1, env = empty_lvl_env, trashCount = -1, exit = True, selectlvl=False}

-- Resets level to its original start position
resetLevel :: Level -> Level
resetLevel lvl = intToLvl (levelNum lvl)