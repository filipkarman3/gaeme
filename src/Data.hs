module Data where

-- what the player is curently doing
data Scene     = Menu     -- picking a level
               | Intro    -- seeing the intro cutscene
               | Gameplay -- actually playing the game
               | Outro    -- finished a series of levels, you see the "you win"