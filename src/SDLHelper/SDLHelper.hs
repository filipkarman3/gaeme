{-# LANGUAGE OverloadedStrings #-}

module SDLHelper.SDLHelper where

import qualified SDL
import qualified SDL.Image

import qualified SDLHelper.Data.WorldExposed as W (World(wr), WorldRaw(..), getKb, logger, clearLog, getR)
import SDLHelper.Data.Rect

import qualified SDLHelper.Data.MiscData  as MD

import qualified SDLHelper.KeyboardReader as KB
import qualified SDLHelper.Data.Keyboard  as KB (Keyboard)

import qualified SDL.Font  as SDLF
import qualified SDL.Mixer as SDLM

import Control.Monad          (void, when)
import Control.Monad.Extra    (loopM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text)

import Foreign.C.Types (CInt, CDouble)

-- check the general setup section of the readme
doMain :: Text
       -> (Int, Int)
       -> FilePath
       -> (W.WorldRaw -> IO W.World)
       -> (W.World -> IO W.World)
       -> (W.World -> IO ())
       -> IO ()
doMain winName (winX, winY) kbPath fInit fLoop fTerminate = withSDL
    $ SDLM.withAudio SDLM.defaultAudio 1024
    $ withWindow winName (winX, winY)
    $ \w -> withRenderer w
    $ \r -> KB.withKeyboard kbPath
    $ \kb -> do
        -- this isn't necessary right now (ie: we don't need to know the kbstate rn)
        -- BUT the kbstate is a parameter of WorldRaw, so we define it here as a dummy variable
        -- it is used to initialise WorldRaw and will get updated on each consecutive frame
        kbState <- SDL.getKeyboardState

        -- SDL.Font needs to be initialised
        SDLF.initialize

        -- define a lot of base datatypes here to prevent the library user having to it themselves
        let raw = W.WorldRaw {
            W.kb   = kb,
            W.kbs  = kbState,
            W.kbps = kbState,
            W.w    = w,
            W.es   = [],
            W.r    = r,
            W.fps  = 50,
            W.quit = False,
            W.logger = []
        }

        world <- fInit raw
        world' <- loop fLoop world
        fTerminate world'

        -- gotta unload some SDL.Font stuff
        SDLF.quit

        -- gotta return the keyboard layout
        pure $ getUpdatedKb world'
    
    where

        getUpdatedKb :: W.World -> KB.Keyboard
        getUpdatedKb world = W.getKb world

-- initialises SDL, runs the game, quits SDL
withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
    SDL.initializeAll
    _ <- op
    SDL.quit

-- creates a window, runs a game with that window, closes the window
withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
    w <- createSizedWindow title (x, y)
    SDL.showWindow w
    void $ op w
    SDL.destroyWindow w

-- creates a window with the given title and dimensions
createSizedWindow :: (MonadIO m) => Text -> (Int, Int) -> m SDL.Window
createSizedWindow title (x, y) = SDL.createWindow title settings where
    settings   = SDL.defaultWindow { SDL.windowInitialSize = resolution }
    resolution = SDL.V2 (fromIntegral x) (fromIntegral y)


-- runs the game with a renderer
withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
    SDL.HintRenderVSync SDL.$= SDL.EnableVSync
    r <- SDL.createRenderer w (-1) rendererConfig
    op r
    SDL.destroyRenderer r

-- sets up the config for the renderer, used to initialise the renderer
-- you can change this to use accelerated hardware and whatnote
-- but the accelerated hardware setting doesn't work on my poopy laptop
rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig {
        SDL.rendererType = SDL.SoftwareRenderer,
        SDL.rendererTargetTexture = False
    }

-- iteratively poll for SDL events, perform some operation, quit upon a QuitEvent
loop :: (MonadIO m)
     => (W.World -> m W.World)
     -> W.World
     -> m W.World
loop op st = loopM (withEventHandling op) st

-- abstracts:
-- - quitting the game,
-- - the left-right datatype wrapping necesitated by loopM,
-- - waiting if the game is running faster than the specified FPS
-- - rendering the renderer buffer to the screen
    -- (the renderer buffer is where you render stuff to, before it all gets put onto the screen)
-- - getting SDL events and storing them in the WorldRaw record
-- - outputs logs created by the user in the WorldRaw record
withEventHandling :: (MonadIO m)
                  => (W.World -> m W.World)
                  -> W.World
                  -> m (Either W.World W.World)
withEventHandling op st = do
    -- get list of SDL events like keypresses
    events <- pollEvents
    state  <- SDL.getKeyboardState
    
    let raw = W.wr st

    -- quit the game if a quit event occurred
    if quitEventOccurred events || W.quit raw then pure $ Right st

    --otherwise, run the game loop
    else do
        -- update the necessary values, like keypresses done this turn
        let raw' = raw { W.es = events, W.kbps = W.kbs raw, W.kbs = state }

        -- then actuall run the frame
        st' <- withTiming (withRendering op) (st { W.wr = raw' })
        
        -- return the game state
        pure $ Left st'

-- used to prevent the game running faster than the specified FPS
-- called in withEventHandling
withTiming :: (MonadIO m) => (W.World -> m W.World) -> W.World -> m W.World
withTiming op st = do
    -- get start time of tick
    starttick <- SDL.ticks

    -- perform game loop
    st' <- op st

    -- get end time of tick
    endtick <- SDL.ticks

    -- keep the game running at specified fps if it's running too fast
    wait $ fromIntegral frameTime - (endtick - starttick)

    pure st'
    where
        frameTime :: Int
        frameTime = 1000 `div` W.fps (W.wr st)
        wait ms = when (20 > ms && ms > 0) $ SDL.delay ms

-- renders the renderer buffer to the screen
withRendering :: (MonadIO m) => (W.World -> m W.World) -> W.World -> m W.World
withRendering op st = do
    -- clear the screen
    SDL.clear $ W.r $ W.wr st

    -- actually run the game tick
    st' <- op st >>= outputLogs

    -- render changes to the screen
    SDL.present $ W.r $ W.wr st'

    pure st'

-- outputs logs
outputLogs :: (MonadIO m) => W.World -> m W.World
outputLogs w = do
    foldr (\s l -> (liftIO . print) s >> l) (pure ()) (W.logger $ W.wr w)
    pure $ W.clearLog w

-- get all the events that have happened since the last time this function was called
pollEvents :: (MonadIO m) => m [SDL.EventPayload]
pollEvents = map SDL.eventPayload <$> SDL.pollEvents

-- check for a quit event
quitEventOccurred :: [SDL.EventPayload] -> Bool
quitEventOccurred es = SDL.QuitEvent `elem` es

----- HELPER FUNCTIONS -----
-- used to simplify loading textures
-- textures are stored as a tuple containing the texture and information about the texture (eg: width)
-- there's a newtype wrapper for this called Sprite in SDLHelper.Data.MiscData
loadTexture :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTexture r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)

-- given a surface, it creates a texture and frees space used up by the surface
surfaceToTexture :: (MonadIO m) => SDL.Renderer -> SDL.Surface -> m MD.Sprite
surfaceToTexture r s = do
    -- create a texture from the surface
    t <- SDL.createTextureFromSurface r s
    i <- SDL.queryTexture t

    -- free up memory taken up by surface
    SDL.freeSurface s

    -- return the texture
    pure (t, i)

-- render stuff onto the screen
renderSimple :: W.World
             -> MD.Sprite
             -> SDL.V2 Int
             -> IO ()
renderSimple w (t, i) (SDL.V2 x y) = SDL.copy (W.getR w) t Nothing (Just rect) where
    rect   = toSDLRect (toCInt x) (toCInt y) width height
    width  = SDL.textureWidth i
    height = SDL.textureHeight i

    toCInt :: Int -> CInt
    toCInt = fromIntegral

-- abstracts renderSimple for instances of Entity
renderEntity :: (MD.Drawable e)
             => W.World
             -> e
             -> IO ()
renderEntity w e = renderSimple w (MD.getSprite e) (MD.getPos e)

destroyTextures :: (Foldable t, MD.Drawable e) => t e -> IO ()
destroyTextures t = foldr (\x io -> SDL.destroyTexture (fst $ MD.getSprite x) >> io) (pure ()) t

destroyTextures' :: (Foldable t) => t MD.Sprite -> IO ()
destroyTextures' t = foldr (\x io -> SDL.destroyTexture (fst x) >> io) (pure ()) t