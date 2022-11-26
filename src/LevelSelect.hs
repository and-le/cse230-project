-- Level Select and various Levels
module LevelSelect
(
    levelSelect,
    resetLevel
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

app :: App Level e Name
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

-- Level Select List for Display
levels = ["Level 0", "Level 1", "Level 2"]
concat_bullet = ("-" ++)
levels_list = map str (map (concat_bullet) levels)

-- Level Select UI
ui :: Widget ()
ui =
    C.center
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Raccoon Rush")
    $ foldl (<=>) (str "Level Select:") levels_list

---- Level Definitions
-- Empty Level
empty_lvl :: Level
empty_lvl = (-1, empty_lvl_env)
empty_lvl_env :: Environment
empty_lvl_env = Data.Matrix.fromLists [[playerCell]]

-- Level 0
level_0 :: Level
level_0 = (0, level_0_env)
level_0_env :: Environment
level_0_env = Data.Matrix.fromLists [[emptyCell , emptyCell     , wallCell       , emptyCell]
                        ,[emptyCell , trashCell   , trashCell      , wallCell]
                        ,[emptyCell, trashCell , playerCell , wallCell]
                        ,[wallCell , wallCell     , wallCell       , emptyCell]
                        ]

-- Level 1
level_1 :: Level
level_1 = (1, level_1_env)
level_1_env :: Environment
level_1_env = Data.Matrix.fromLists [[trashCell , trashCell     , wallCell       , emptyCell]
                        ,[emptyCell , trashCell   , trashCell      , wallCell]
                        ,[emptyCell, trashCell , playerCell , wallCell]
                        ,[wallCell , wallCell     , wallCell       , emptyCell]
                        ]

-- Level 2
level_2 :: Level
level_2 = (2, level_2_env)
level_2_env :: Environment
level_2_env = Data.Matrix.fromLists [[wallCell , wallCell     , wallCell       , emptyCell]
                        ,[emptyCell , trashCell   , trashCell      , wallCell]
                        ,[emptyCell, trashCell , playerCell , wallCell]
                        ,[wallCell , wallCell     , wallCell       , emptyCell]
                        ]

-- Event Handling for Level Select Screen
handleEvent :: Level -> BrickEvent () e -> EventM Name (Next Level)
-- Exiting Level Select
handleEvent env (VtyEvent (V.EvKey V.KEsc [])) = halt env
handleEvent env (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt env
-- Level Select Event
handleEvent env (VtyEvent (V.EvKey (V.KChar d) [])) =
  if d `elem` ['0' .. '2'] then
    do
        case read [d] of
            0 -> halt $ level_0
            1 -> halt $ level_1
            2 -> halt $ level_2
            _ -> continue env
  else continue env
handleEvent env _                              = continue env

-- Level Select Screen
levelSelect = defaultMain app empty_lvl

-- Resets level to its original start position
resetLevel :: Level -> Level
resetLevel lvl =
    case (fst lvl) of
        0 -> level_0
        1 -> level_1
        2 -> level_2
        _ -> empty_lvl