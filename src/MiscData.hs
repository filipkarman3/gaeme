module MiscData where

import qualified LevelData as L

-- what the player is curently doing
data Scene     = Menu     -- picking a level
               | Intro    -- seeing the intro cutscene
               | Gameplay -- actually playing the game
               | Outro    -- finished a series of levels, you see the "you win"

-- contains all the series in the game
-- Int is the index of the series that is being focused on whilst in the menu
data MenuData  = MenuData Int [L.Series]