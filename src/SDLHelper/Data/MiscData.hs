module SDLHelper.Data.MiscData where

import qualified SDL

import SDLHelper.Data.Rect (Rect, rectX, rectY)

type Sprite = (SDL.Texture, SDL.TextureInfo)

class Drawable a where
    getRect :: a -> Rect
    setRect :: a -> Rect -> a

    getPos :: a -> SDL.V2 Int
    getPos a = SDL.V2 x y where
        x = toInt $ rectX r
        y = toInt $ rectY r
        r = getRect a
    
    changeX :: a -> Float -> a
    changeX a d = setRect a r' where
        r  = getRect a
        r' = r { rectX = rectX r + d }
    
    setX :: a -> Float -> a
    setX a d = setRect a r' where
        r = getRect a
        r' = r {rectX = d}

    changeY :: a -> Float -> a
    changeY a d = setRect a r' where
        r  = getRect a
        r' = r { rectY = rectY r + d }
    
    setY :: a -> Float -> a
    setY a d = setRect a r' where
        r = getRect a
        r' = r {rectY = d}

    changePos :: a -> (Float, Float) -> a
    changePos a (x, y) = changeX (changeY a y) x

    setPos :: a -> (Float, Float) -> a
    setPos a (x,y) = setX (setY a y) x

    getSprite :: a -> (SDL.Texture, SDL.TextureInfo)

toInt :: Float -> Int
toInt = round

ifM :: (Monad m) => m Bool -> a -> a -> m a
ifM cond x y = cond >>= \b -> if b then pure x else pure y

data Drawthing = Drawthing {
    rect :: Rect,
    sprite :: Sprite
}

instance Show Drawthing where
    show d = "Drawthing with rect: " ++ show (rect d)

instance Drawable Drawthing where
    getRect = rect
    setRect b r = b { rect = r }
    getSprite = sprite