{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib

import qualified SDL
import qualified SDL.Font as SDLF

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

import qualified Data.StateVar as S

import Control.Monad.Extra (when)

screenWidth = 500
screenHeight = 500

-- ## -- ## -- ## -- ## MAIN ## -- ## -- ## -- ## --
main :: IO ()
main = H.doMain "Gaeme" (screenWidth, screenHeight) "assets/layout.kb" gameInit gameLoop gameTerminate

-- ## -- ## -- ## -- ## GAME INIT ## -- ## -- ## -- ## --
-- initialises variables necessary for the game
gameInit :: W.WorldRaw -> IO W.World
gameInit wr = do
    let tileWidth = 25 -- the width of the tiles on the screen

    let dirstrs = ["left", "right", "up", "down"]

    arrowSprites         <- framesToMap [L.Left ..] $ loadFrames wr "assets/arrows/" dirstrs ".png"
    arrowSpritesInactive <- framesToMap [L.Left ..] $ loadFrames wr "assets/arrows/" dirstrs "-inactive.png"
    playerSprites        <- framesToMap [L.Left ..] $ loadFrames wr "assets/player/" dirstrs ".png"
    tileSprites          <- framesToMap [L.Box [], L.Wall, L.Hole, L.FilledHole] $ loadFrames wr "assets/tiles/" ["box", "wall", "hole", "filledHole"] ".png"

    font <- SDLF.load "assets/font.ttf" 14

    let p = P.Player {
        P.rect = R.Rect 0 0 25 25,
        P.sprites = playerSprites,
        P.dir = L.Left
    }

    signSprite  <- H.loadTexture (W.r wr) "assets/sign.png"
    introImg    <- H.loadTexture (W.r wr) "assets/intro.png"
    floorSprite <- H.loadTexture (W.r wr) "assets/floor.png"
    cheeseSprite <- H.loadTexture (W.r wr) "assets/cheese.png"
    endingImg    <- H.loadTexture (W.r wr) "assets/ending.png"

    -- create the world
    let w = W.World {
        W.wr = wr,
        W.tileWidth = tileWidth,
        W.player = MD.changePos p (mapTuple fromIntegral L.playerStart),
        W.tileSprites = tileSprites,
        W.levels = L.levels,
        W.savedLevel = L.levels !! 7,
        W.savedPlayerPos = L.playerStart,
        W.levelNum = 7,
        W.moveNum = 0,
        W.arrowSprites = arrowSprites,
        W.arrowSpritesInactive = arrowSpritesInactive,
        W.signSprite = signSprite,
        W.floorSprite = floorSprite,
        W.signPos = [(10,5),(10,3),(13,6),(9,9),(13,5),(4,5), (3,18),(3,18)],
        W.tick = 0,
        W.font = font,
        W.isIntro = True,
        W.introImg = introImg,
        W.isEnding = False,
        W.endingImg = endingImg,
        W.cheeseSprite = cheeseSprite,
        W.cheesePos = (12,4)
    }

    pure w -- return the world

framesToMap :: (Ord k) => [k] -> IO [MD.Sprite] -> IO (Map.Map k MD.Sprite)
framesToMap ks iovs = iovs >>= (\vs -> pure $ Map.fromList $ zip ks vs)

loadFrames :: W.WorldRaw -> String -> [String] -> String -> IO [MD.Sprite]
loadFrames wr s1 ss s2 = mapM (H.loadTexture (W.r wr)) ss' where
    ss' = map (\s -> s1 ++ s ++ s2) ss

-- ## -- ## -- ## -- ## GAME LOOP ## -- ## -- ## -- ## --
-- checks if quit key is pressed. If so, quits
-- if not, first tick the world, then update the screen accordingly
gameLoop :: W.World -> IO W.World
gameLoop w = ifM (KB.isKeyPressed w KB.Quit)
        {-then-} (pure $ W.setQuit w True)
        {-else-} (if W.isIntro w then introTick w
            else if W.isEnding w then outroTick w
            else tickWorld w >>= \w' -> renderWorld w' >> pure w')

introTick :: W.World -> IO W.World
introTick w = ifM (KB.isKeyPressed w KB.Restart) (pure $ w { W.isIntro = False }) (H.renderSimple w (W.introImg w) (SDL.V2 0 0) >> pure w)

outroTick :: W.World -> IO W.World
outroTick w = ifM (KB.isKeyPressed w KB.Quit) (pure $ W.setQuit w True) (H.renderSimple w (W.endingImg w) (SDL.V2 0 0) >> pure w)

-- ## -- ## -- ## -- ## TICK WORLD ## -- ## -- ## -- ## --
tickWorld :: W.World -> IO W.World
tickWorld w = movePlayer w >>= restartLevel >>= changeLevel >>= rewind >>= tick >>= checkEnd

tick :: W.World -> IO W.World
tick w = pure $ w { W.tick = W.tick w + 1 }

checkEnd :: W.World -> IO W.World
checkEnd w = pure $ if W.levelNum w == 5 && getPlayerPos w == W.cheesePos w then w { W.isEnding = True} else w
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

restartLevel :: W.World -> IO W.World
restartLevel w = ifM (KB.isKeyPressed w KB.Restart) (pure levelReset) (pure w) where
    levelReset = w {
        W.levels = setAt (W.levels w) (W.levelNum w) (W.savedLevel w),
        W.player = MD.setPos (W.player w) (mapTuple fromIntegral (W.savedPlayerPos w))
    }

changeLevel :: W.World -> IO W.World
changeLevel w
    | toofarright = pure wNext
    | toofarleft  = pure wSame
    | otherwise   = pure w where
        wNext = w {
            -- W.levelNum = if W.levelNum w + 1 >= length (W.levels w) then length (W.levels w)-1 else (W.levelNum w + 1),
            W.levelNum = ln + 1,
            W.savedLevel = W.levels w !! (ln + 1),
            W.savedPlayerPos = (0,snd $ getPlayerPos w),
            W.player   = pLeft
        }
        wSame = w {
            W.player   = pLeft
        }
        pLeft       = MD.setX (W.player w) 0
        pRight      = MD.setX (W.player w) (fromIntegral lastTile)
        toofarleft  = fst (getPlayerPos w) < 0
        toofarright = fst (getPlayerPos w) > lastTile
        lastTile    = screenWidth `div` (W.tileWidth w) - 1
        ln          = W.levelNum w


getPlayerPos :: W.World -> (Int,Int)
getPlayerPos w = (x,y) where
    SDL.V2 x y = MD.getPos $ W.player w

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
    H.renderSimple w (W.signSprite w) (SDL.V2 signX signY)
    renderText w
    when (W.levelNum w == 5) (H.renderSimple w (W.cheeseSprite w) ((uncurry SDL.V2) $ mapTuple (W.tileWidth w *) (W.cheesePos w)))
    where
        tw = W.tileWidth w
        (signX, signY) = mapTuple (tw*) $ W.signPos w !! W.levelNum w

renderText :: W.World -> IO ()
renderText w = if W.levelNum w == 0 then do
        blit (10,2) "Arrow keys to move" (SDL.V4 0 0 0 (fadein 0))
        blit (2,12) "Space to rewind"    (SDL.V4 0 0 0 (fadein 5))
        blit (6,15) "Enter to restart"   (SDL.V4 0 0 0 (fadein 10))
    else pure ()
    where
        f = W.font w
        t = W.tick w
        blit pos s c = do
            surface <- SDLF.blended f c s
            texture <- H.surfaceToTexture (W.getR w) surface
            H.renderSimple w texture (uncurry SDL.V2 $ mapTuple (tw*) pos)
        tw = W.tileWidth w
        fadein  n = fromIntegral $ float2Int $ fadein' n
        fadein' n
            | t < n*50     = 1
            | t < (n+3)*50 = if 255*(fromIntegral (t-(n*50)) / 150.0) == 0 then 1 else 255*(fromIntegral (t-(n*50)) / 150.0)
            | otherwise    = 255

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
    Map.foldr (\t io -> SDL.destroyTexture (fst t) >> io) (pure ()) (W.tileSprites w)
    Map.foldr (\t io -> SDL.destroyTexture (fst t) >> io) (pure ()) (W.arrowSprites w)
    Map.foldr (\t io -> SDL.destroyTexture (fst t) >> io) (pure ()) (W.arrowSpritesInactive w)
    foldr     (\t io -> SDL.destroyTexture (fst t) >> io) (pure ()) (P.sprites $ W.player w)
    SDL.destroyTexture $ fst $ W.signSprite w
    SDLF.free $ W.font w
    SDL.destroyTexture $ fst $ W.introImg w
    SDL.destroyTexture $ fst $ W.cheeseSprite w
    pure ()