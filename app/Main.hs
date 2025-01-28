module Main (main) where

import Lib

import qualified SDL

import qualified SDLHelper.SDLHelper as H
import qualified SDLHelper.Data.WorldExposed as W
import qualified SDLHelper.Data.Rect as R
import qualified SDLHelper.Data.Keyborad as KB
import qualified SDLHelper.Data.KeyboardReaderExposed as KB (Keybind(..))

import qualified Player as P

import Control.Monad.Extra (ifM)

main :: IO ()
main = H.doMain "Gaeme" (500,500) "assets/layout.kb" gameInit gameLoop gameTerminate

gameInit :: W.WorldRaw -> IO W.World
gameInit wr = do
    let tileWidth = 25

    -- create the player object
    let p = P.Player {
        rect = R.Rect 0 0 25 25,
        sprite = H.loadTexture (r wr) "assets/player.png"
    }

    -- create the world
    let w = W.World {
        wr = wr,
        tileWidth = tileWidth,
        player = p
    }

    -- return the world
    pure w

gameLoop :: W.World -> IO ()
gameLoop w = ifM KB.isKeyPressed w KB.Quit
        {-then-} W.setQuit w True
        {-else-} tickWorld w >>= renderWorld

tickWorld :: W.World -> IO W.World
tickWorld w = pure w

movePlayer :: W.World -> IO W.World
movePlayer w = f [KB.Left, KB.Up, KB.Right, KB.Down] [(-1, 0), (0, -1), (1, 0), (0, 1)] where
    f [] _ = w
    f (x:xs) (y:ys) = if KB.isKeyPressed w x
        then adjustPlayer w y
        else f xs ys
    adjustPlayer w (x, y) = w {
        W.player = R.changeX (x * tw) $ R.changeY (y * tw) (W.player w)
    }
    tw = W.tileWidth w

renderWorld :: W.World -> IO W.World
renderWorld w = do
    H.renderEntity w (W.player w)

gameTerminate :: W.World -> IO ()
gameTerminate w = do
    SDL.destroyTexture (fst $ P.sprite $ W.player w)
    pure ()