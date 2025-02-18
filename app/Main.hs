{-# LANGUAGE OverloadedStrings #-}


{- todo:
fix level.hs bc changed how levels are handled (with series inside of world record)
    make it so that L.savedLevel is edited and when reset, bring in level from L.ls !! L.currLevel
-}

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
import qualified Data as D

import GHC.Float (float2Int, int2Float)
import Control.Monad.Extra (ifM)
import Data.Text (pack)
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
    titleSprite <- H.loadTexture (W.r wr) "assets/title.png"
    starSprite <- H.loadTexture (W.r wr) "assets/star.png"

    -- create the world
    let w = W.World {
        W.wr = wr,

        W.player = p,
        W.moveNum = 0,

        W.tick = 0,

        W.tileWidth = tileWidth,
        W.tileSprites = tileSprites,
        W.arrowSprites = arrowSprites,
        W.arrowSpritesInactive = arrowSpritesInactive,
        W.floorSprite = floorSprite,
        W.signSprite = signSprite,
        W.introImg = introImg,
        W.endingImg = endingImg,
        W.cheeseSprite = cheeseSprite,
        W.titleSprite = titleSprite,
        W.starSprite = starSprite,

        W.font = font,

        W.scene = D.Menu,
        W.menuData = L.initMenuData,
        W.curSer = L.dummySeries
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
gameLoop w = tickWorld w >>=/ renderWorld
    
    -- ifM (KB.isKeyPressed w KB.Quit)
    --     {-then-} (pure $ W.setQuit w True)
    --     {-else-} (tickWorld w >>=/ renderWorld)

(>>=/) :: (Monad m) => m a -> (a -> m b) -> m a
x >>=/ y = do
    a <- x
    y a
    pure a

introTick :: W.World -> IO W.World
introTick w = ifM (KB.isKeyPressed w KB.Enter)
        {-then-} (pure $ w { W.scene = D.Gameplay })
        {-else-} (H.renderSimple w (W.introImg w) (SDL.V2 0 0) >> pure w)

outroTick :: W.World -> IO W.World
outroTick w = ifM (KB.isKeyPressed w KB.Enter)
        {-then-} (pure $ W.setQuit w True)
        {-else-} (H.renderSimple w (W.endingImg w) (SDL.V2 0 0) >> pure w)

-- ## -- ## -- ## -- ## TICK WORLD ## -- ## -- ## -- ## --
tickWorld :: W.World -> IO W.World
tickWorld w = case W.scene w of
    D.Intro    -> introTick w
    D.Outro    -> outroTick w
    D.Gameplay -> gameTick w
    D.Menu     -> menuTick w

menuTick :: W.World -> IO W.World
menuTick w = quit w >>= moveFocusUp >>= moveFocusDown >>= selectFocus where
    quit          = whenM (KB.isKeyPressed w KB.Quit)  (\w' -> W.setQuit w' True)
    moveFocusUp   = whenM (KB.isKeyPressed w KB.Up)    moveFocusUp'
    moveFocusDown = whenM (KB.isKeyPressed w KB.Down)  moveFocusDown'
    selectFocus   = whenM (KB.isKeyPressed w KB.Enter) selectFocus'

whenM :: (Monad m) => m Bool -> (a -> a) -> (a -> m a)
whenM cond f = \a -> ifM cond (pure (f a)) (pure a)

moveFocusUp' :: W.World -> W.World
moveFocusUp' w = w { W.menuData = m' } where
    L.MenuData i ls = W.menuData w
    m' = L.MenuData i' ls
    i' = if i == 0 then length ls-1 else i-1

moveFocusDown' :: W.World -> W.World
moveFocusDown' w = w { W.menuData = m' } where
    L.MenuData i ls = W.menuData w
    m' = L.MenuData i' ls
    i' = if i == length ls-1 then 0 else i+1

selectFocus' :: W.World -> W.World
selectFocus' w = w {
        W.scene  = D.Intro,
        W.curSer = L.loadSeries s,
        W.player = MD.setPos (W.player w) (mapTuple int2Float (L.startPos s))
    } where
        s = ss!!focus
        L.MenuData focus ss = W.menuData w

gameTick :: W.World -> IO W.World
gameTick w = movePlayer w >>= restartLevel >>= changeLevel >>= rewind >>= tick >>= checkEnd >>= checkExit

tick :: W.World -> IO W.World
tick w = pure $ w { W.tick = W.tick w + 1 }

checkEnd :: W.World -> IO W.World
checkEnd w = if finalLevel then pure (w {W.scene = D.Outro}) else pure w where
    finalLevel = length (L.ls s)-1 == L.currentLevel s && getPlayerPos w == L.cheesePos s
    s = W.curSer w
    -- pure $ if W.levelNum w == 5 && getPlayerPos w == W.cheesePos w then w { W.isEnding = True} else w

checkExit :: W.World -> IO W.World
checkExit w = whenM (KB.isKeyPressed w KB.Quit) goToMenu w

goToMenu :: W.World -> W.World
goToMenu w = w { W.scene = D.Menu }

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
        -- W.levels = setAt (W.levels w) (W.levelNum w) (W.savedLevel w),
        W.curSer = s { L.savedLevel = L.ls s !! L.currentLevel s },
        W.player = MD.setPos (W.player w) (mapTuple fromIntegral (L.savedPlayerPos s))
    }
    s = W.curSer w

changeLevel :: W.World -> IO W.World
changeLevel w
    | toofarright = pure (if isFinalLevel then wSame' else wNext)
    | toofarleft  = pure wSame
    | otherwise   = pure w where
        wNext = w {
            -- W.levelNum = ln + 1,
            -- W.savedLevel = W.levels w !! (ln + 1),
            -- W.savedPlayerPos = (0,snd $ getPlayerPos w),
            W.curSer = s {
                L.currentLevel   = L.currentLevel s + 1,
                L.savedLevel     = L.ls s !! (L.currentLevel s +1),
                L.savedPlayerPos = (0,snd $ getPlayerPos w)
            },
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
        s           = W.curSer w
        isFinalLevel = L.currentLevel s == length (L.ls s)-1
        wSame'      = w { W.player = MD.setX (W.player w) 19 }


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
level w = L.savedLevel $ W.curSer w

mapTuple :: (a->b) -> (a,a) -> (b,b)
mapTuple f (a,b) = (f a, f b)

-- ## -- ## -- ## -- ## RENDER WORLD ## -- ## -- ## -- ## --
renderWorld :: W.World -> IO ()
renderWorld w = do
    -- fill screen with white (background)
    SDL.rendererDrawColor (W.getR w) SDL.$= SDL.V4 255 255 255 255
    SDL.fillRect (W.getR w) Nothing

    -- foreground is scene-dependent
    case W.scene w of
        D.Menu     -> renderMenu w
        D.Intro    -> renderIntro w
        D.Gameplay -> renderGamePlay w
        D.Outro    -> renderOutro w

renderMenu :: W.World -> IO ()
renderMenu w = renderTitle >> renderLevels >> renderInstructions >> renderNameText >> renderDiffText where
    renderTitle  = H.renderSimple w (W.titleSprite w) (SDL.V2 0 0)
    renderLevels = mapM_ (\n -> renderLevelInMenu w (ss!!adj n) n) [-2..2]
    renderInstructions = blit (10,450) "Arrows to change level, Enter to select"
    renderNameText     = blit (10,190) "Level name:"
    renderDiffText     = blit (300,190) "Difficulty:"

    blit pos s = do
            surface <- SDLF.blended (W.font w) (SDL.V4 0 0 0 255) (pack s)
            texture <- H.surfaceToTexture (W.getR w) surface
            H.renderSimple w texture (tup2SDLV2 pos)

    adj n = (n+i) `mod` (length ss)
    L.MenuData i ss = W.menuData w
    
renderLevelInMenu :: W.World -> L.Series -> Int -> IO ()
renderLevelInMenu w s pos = do
    blit (xPad, pos) (L.name s)
    -- blit (xPad, {-calcY pos-}300) (L.name s)
    blitStars (xPad', calcY pos) (L.difficulty s)
    where
        blit :: (Int, Int) -> String -> IO ()
        blit (x,y) s' = do
            surface <- SDLF.blended (W.font w) (SDL.V4 0 0 0 (fromIntegral $ calcOpacity y)) (pack s')
            texture <- H.surfaceToTexture (W.getR w) surface
            H.renderSimple w texture (tup2SDLV2 (x, calcY y))
        
        calcY :: Int -> Int
        calcY y = centre - (h`div`2) + y*h

        h = 30
        centre = ((screenHeight-100)`div`2) + 100

        calcOpacity :: Int -> Int
        calcOpacity y = 255 - (abs y)*(255`div`3)

        blitStars :: (Int, Int) -> Int -> IO ()
        blitStars (x,y) n = mapM_ (\n' -> blitStar (x,y) n') [0..n-1]
        
        blitStar :: (Int, Int) -> Int -> IO ()
        blitStar (x,y) n' = H.renderSimple w (W.starSprite w) (tup2SDLV2 (x+starSpacing*n',y))

        xPad = 10
        xPad' = screenWidth `div` 2
        starSpacing = 43

tup2SDLV2 :: (Int, Int) -> SDL.V2 Int
tup2SDLV2 tup = (uncurry SDL.V2) tup

renderIntro :: W.World -> IO ()
renderIntro w = H.renderSimple w (W.introImg w) (SDL.V2 0 0)

renderGamePlay :: W.World -> IO ()
renderGamePlay w = do
    L.renderLevel w (level w)
    renderTileEntity w (W.player w)
    H.renderSimple w (W.signSprite w) (tup2SDLV2 signPos)
    renderText w
    when isFinalLevel (H.renderSimple w (W.cheeseSprite w) ((uncurry SDL.V2) $ mapTuple (W.tileWidth w *) (L.cheesePos s)))
    where
        tw = W.tileWidth w
        signPos = mapTuple (tw*) $ L.signPos s !! L.currentLevel s
        s = W.curSer w
        isFinalLevel = L.currentLevel s == length (L.ls s) -1

renderText :: W.World -> IO ()
renderText w = when (L.currentLevel s == 0 && L.name s == "base") $ do
        blit (10,2) "Arrow keys to move" (SDL.V4 0 0 0 (fadein 0))
        blit (2,12) "Space to rewind"    (SDL.V4 0 0 0 (fadein 5))
        blit (6,15) "Enter to restart"   (SDL.V4 0 0 0 (fadein 10))
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
        s = W.curSer w

renderOutro :: W.World -> IO ()
renderOutro w = H.renderSimple w (W.endingImg w) (SDL.V2 0 0)

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
    H.destroyTextures' $ W.tileSprites w
    H.destroyTextures' $ W.arrowSprites w
    H.destroyTextures' $ W.arrowSpritesInactive w
    H.destroyTextures' $ P.sprites $ W.player w

    SDLF.free $ W.font w
    SDL.destroyTexture $ fst $ W.introImg w
    SDL.destroyTexture $ fst $ W.signSprite w
    SDL.destroyTexture $ fst $ W.cheeseSprite w
    SDL.destroyTexture $ fst $ W.titleSprite w
    SDL.destroyTexture $ fst $ W.starSprite w
    pure ()