-- Level Select and various Levels
module LevelSelect
(
    levelSelect,
    resetLevel,
    max_level
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
levels = [("Level 0",0), ("Level 1",1), ("Level 2",2)]
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
    [ C.center $ vBox
      [ C.center 
          $ withBorderStyle BS.unicodeBold
          $ B.borderWithLabel (str "Raccoon Rush")
          $ foldl (<=>) (str "Level Select:") (levels_list (highlighted env))
        , str "Use arrow and enter keys, or type the number to select a level, esc/q to exit"
      ]
    ]

-- Level Select Attr map
lvlAttrMap :: AttrMap
lvlAttrMap = attrMap V.defAttr
  [ (attrName "selected", fg V.blue `V.withStyle` V.bold) ]

---- Level Definitions
max_level :: Int
max_level = 2
-- Empty Level
empty_lvl :: Level
empty_lvl = MkLevel {levelNum=(-1), env=empty_lvl_env, trashCount=(-1), exit=False}
empty_lvl_env :: Environment
empty_lvl_env = Data.Matrix.fromLists [[playerCell]]

-- Level 0
level_0 :: Level
level_0 = MkLevel {levelNum=(0), env=level_0_env, trashCount=getTrashCount level_0_env, exit=False}
level_0_env :: Environment
-- level_0_env = Data.Matrix.fromLists [[emptyCell , emptyCell     , wallCell       , emptyCell]
--                         ,[emptyCell , trashCell   , trashCell      , wallCell]
--                         ,[emptyCell, trashCell , playerCell , wallCell]
--                         ,[wallCell , wallCell     , wallCell       , emptyCell]
--                         ]

level_0_env = Data.Matrix.fromLists [[emptyCell , emptyCell, emptyCell]
                        ,[stashCell , trashCell   , playerCell]
                        ,[emptyCell, emptyCell , emptyCell]
                        ]

-- Level 1
level_1 :: Level
level_1 = MkLevel {levelNum=(1), env=level_1_env, trashCount=getTrashCount level_1_env, exit=False}
level_1_env :: Environment
-- level_1_env = Data.Matrix.fromLists [[trashCell , trashCell     , wallCell       , emptyCell]
--                         ,[emptyCell , trashCell   , trashCell      , wallCell]
--                         ,[emptyCell, trashCell , playerCell , wallCell]
--                         ,[wallCell , wallCell     , wallCell       , emptyCell]
--                         ]

level_1_env = Data.Matrix.fromLists [[emptyCell , emptyCell, emptyCell]
                        ,[playerCell , trashCell   , stashCell]
                        ,[emptyCell, emptyCell , emptyCell]
                        ]

-- Level 2
level_2 :: Level
level_2 = MkLevel {levelNum=(2), env=level_2_env, trashCount=getTrashCount level_2_env, exit=False}
level_2_env :: Environment
-- level_2_env = Data.Matrix.fromLists [[wallCell , wallCell     , wallCell       , emptyCell]
--                         ,[emptyCell , trashCell   , trashCell      , wallCell]
--                         ,[emptyCell, trashCell , playerCell , wallCell]
--                         ,[wallCell , wallCell     , wallCell       , emptyCell]
--                         ]


level_2_env = Data.Matrix.fromLists [[emptyCell , playerCell, emptyCell]
                        ,[emptyCell , trashCell   , stashCell]
                        ,[emptyCell, stashCell , emptyCell]
                        ]

-- Event Handling for Level Select Screen
handleEvent :: Selector -> BrickEvent () e -> EventM Name (Next Selector)
-- Exiting Level Select
handleEvent env (VtyEvent (V.EvKey V.KEsc [])) = halt env
handleEvent env (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt env
-- Level Select Event
handleEvent env (VtyEvent (V.EvKey (V.KChar d) [])) =
  if d `elem` ['0' .. '2'] then
    do
        case read [d] of
            0 -> halt $ MkSelector {highlighted=0}
            1 -> halt $ MkSelector {highlighted=1}
            2 -> halt $ MkSelector {highlighted=2}
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
        _ -> empty_lvl

-- Resets level to its original start position
resetLevel :: Level -> Level
resetLevel lvl = intToLvl (levelNum lvl)