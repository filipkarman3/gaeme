module Player where

import qualified SDLHelper.Data.MiscData as MD

import SDLHelper.Data.Rect

import qualified LevelData as L

import qualified Data.Map as Map

import qualified SDL

data Player = Player {
    rect  :: Rect,
    sprites :: Map.Map L.Dir MD.Sprite,
    dir :: L.Dir
}

instance MD.Drawable Player where
    getRect = rect
    setRect p r = p { rect = r }
    getSprite p = sprites p Map.! dir p