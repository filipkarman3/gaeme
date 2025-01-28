{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib

import qualified SDL

import qualified SDLHelper.SDLHelper as H
import qualified SDLHelper.Data.WorldExposed as W
import qualified SDLHelper.Data.Rect as R
import qualified SDLHelper.KeyboardReader as KB
import qualified SDLHelper.Data.MiscData as MD
import qualified SDLHelper.Data.KeyboardReaderExposed as KB (Keybind(..))

import qualified Player as P

import Control.Monad.Extra (ifM)

-- ## -- ## -- ## -- ## MAIN ## -- ## -- ## -- ## --
main :: IO ()
main = H.doMain "Gaeme" (500,500) "assets/layout.kb" gameInit gameLoop gameTerminate

-- ## -- ## -- ## -- ## GAME INIT ## -- ## -- ## -- ## --
-- initialises variables necessary for the game
gameInit :: W.WorldRaw -> IO W.World
gameInit wr = do
    let tileWidth = 25.0 -- the width of the tiles on the screen

    -- create the player object
    playerSprite <- H.loadTexture (W.r wr) "assets/player.png"
    let p = P.Player {
        P.rect = R.Rect 0 0 25 25,
        P.sprite = playerSprite
    }

    -- create the world
    let w = W.World {
        W.wr = wr,
        W.tileWidth = tileWidth,
        W.player = p
    }

    pure w -- return the world

-- ## -- ## -- ## -- ## GAME LOOP ## -- ## -- ## -- ## --
-- checks if quit key is pressed. If so, quits
-- if not, first tick the world, then update the screen accordingly
gameLoop :: W.World -> IO W.World
gameLoop w = ifM (KB.isKeyPressed w KB.Quit)
        {-then-} (pure $ W.setQuit w True)
        {-else-} (tickWorld w >>= \w' -> renderWorld w' >> pure w')

-- ## -- ## -- ## -- ## TICK WORLD ## -- ## -- ## -- ## --
tickWorld :: W.World -> IO W.World
tickWorld w = movePlayer w

-- move the player on the screen
movePlayer :: W.World -> IO W.World
movePlayer w = f keys dirs where
    keys = [KB.Left, KB.Up, KB.Right, KB.Down]
    dirs = map (\(x,y)->(x*tw,y*tw)) [(-1.0, 0.0), (0.0, -1.0), (1.0, 0.0), (0.0, 1.0)]
    tw = W.tileWidth w

    -- for each key [Left, Up, Right, Down], check if it is pressed
    -- if it is, adjust the player's position
    -- otherwise, check the next key
    f :: [KB.Keybind] -> [(Float, Float)] -> IO W.World
    f [] _ = pure w
    f (x:xs) (y:ys) = ifM (KB.isKeyPressed w x)
        {-then-} (pure (adjustPlayer y))
        {-else-} (f xs ys)

    -- each key has a corresponding x and y, eg: left -> (-1, 0)
    -- adjustPlayer moves the player by the corresponding x and y
    adjustPlayer :: (Float, Float) -> W.World
    adjustPlayer dir = w {
        W.player = MD.changePos (W.player w) dir
    }

-- ## -- ## -- ## -- ## RENDER WORLD ## -- ## -- ## -- ## --
renderWorld :: W.World -> IO ()
renderWorld w = do
    SDL.rendererDrawColor (W.getR w) SDL.$= SDL.V4 255 255 255 255
    SDL.fillRect (W.getR w) Nothing
    H.renderEntity w (W.player w)

-- ## -- ## -- ## -- ## GAME TERMINATE ## -- ## -- ## -- ## --
-- free any memory here
gameTerminate :: W.World -> IO ()
gameTerminate w = do
    SDL.destroyTexture (fst $ P.sprite $ W.player w)
    pure ()