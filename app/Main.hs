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

import qualified Data.Map as Map
import qualified Level as L
import qualified LevelData as L

import qualified Player as P

import GHC.Float (float2Int)
import Control.Monad.Extra (ifM)

-- ## -- ## -- ## -- ## MAIN ## -- ## -- ## -- ## --
main :: IO ()
main = H.doMain "Gaeme" (500,500) "assets/layout.kb" gameInit gameLoop gameTerminate

-- ## -- ## -- ## -- ## GAME INIT ## -- ## -- ## -- ## --
-- initialises variables necessary for the game
gameInit :: W.WorldRaw -> IO W.World
gameInit wr = do
    let tileWidth = 25 -- the width of the tiles on the screen

    -- create the player object
    playerSprite <- H.loadTexture (W.r wr) "assets/player.png"
    let p = P.Player {
        P.rect = R.Rect 0 0 25 25,
        P.sprite = playerSprite
    }

    -- load the tile sprites
    boxTexture <- H.loadTexture (W.r wr) "assets/box.png"
    wallTexture <- H.loadTexture (W.r wr) "assets/wall.png"
    holeTexture <- H.loadTexture (W.r wr) "assets/hole.png"
    filledHoleTexture <- H.loadTexture (W.r wr) "assets/filledHole.png"

    let tileSprites = Map.fromList [
            (L.Box [],     boxTexture),
            (L.Wall,       wallTexture),
            (L.Hole,       holeTexture),
            (L.FilledHole, filledHoleTexture)
            ]

    -- create the world
    let w = W.World {
        W.wr = wr,
        W.tileWidth = tileWidth,
        W.player = MD.changePos p (mapTuple fromIntegral L.playerStart),
        W.tileSprites = tileSprites,
        W.levels = L.levels,
        W.levelNum = 0,
        W.moveNum = 0,
        W.moves = 0
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
tickWorld w = movePlayer w >>= rewind

-- move the player on the screen
movePlayer :: W.World -> IO W.World
movePlayer w = f keys dirs where
    keys = [KB.Left, KB.Up, KB.Right, KB.Down]
    dirs = [L.Left, L.Up, L.Right, L.Down]

    -- for each key [Left, Up, Right, Down], check if it is pressed
    -- if it is, adjust the player's position
    -- otherwise, check the next key
    f :: [KB.Keybind] -> [L.Dir] -> IO W.World
    f [] _ = pure w
    f (x:xs) (d:ds) = ifM (KB.isKeyPressed w x)
        {-then-} (pure $ L.movePlayer w d)
        {-else-} (f xs ds)

-- rewinds the boxes once
rewind :: W.World -> IO W.World
rewind w = ifM (KB.isKeyPressed w KB.Rewind) (pure $ L.rewind (decrementWorld w)) (pure w)

decrementWorld :: W.World -> W.World
decrementWorld w = if mN > 0 then w' else w where
    mN = W.moveNum w
    w' = w { W.moveNum = mN-1 }


setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

level :: W.World -> L.Level
level w = W.levels w !! W.levelNum w

mapTuple :: (a->b) -> (a,a) -> (b,b)
mapTuple f (a,b) = (f a, f b)

-- ## -- ## -- ## -- ## RENDER WORLD ## -- ## -- ## -- ## --
renderWorld :: W.World -> IO ()
renderWorld w = do
    SDL.rendererDrawColor (W.getR w) SDL.$= SDL.V4 255 255 255 255
    SDL.fillRect (W.getR w) Nothing
    L.renderLevel w (level w)
    renderTileEntity w (W.player w)

-- ## -- ## -- ## -- ## RENDER TILE ENTITY ## -- ## -- ## -- ## --
-- though I am treating each square as one unit long, this doesn't mean they are one pixel long
-- so you need to scale the position of the rendered entity by the tile width
renderTileEntity :: (MD.Drawable e) => W.World -> e -> IO ()
renderTileEntity w e = H.renderEntity w scaledE where
    scaledE = MD.setRect e r'
    r' = r { R.rectX = fromIntegral tw * R.rectX r, R.rectY = fromIntegral tw * R.rectY r }
    r = MD.getRect e
    tw = W.tileWidth w

-- ## -- ## -- ## -- ## GAME TERMINATE ## -- ## -- ## -- ## --
-- free any memory here
gameTerminate :: W.World -> IO ()
gameTerminate w = do
    SDL.destroyTexture (fst $ P.sprite $ W.player w)
    Map.foldr (\t io -> SDL.destroyTexture (fst $ t) >> io) (pure ()) (W.tileSprites w)
    pure ()