module Level where

import Prelude hiding (Left, Right)

import qualified SDL

import qualified SDLHelper.Data.MiscData as MD
import qualified SDLHelper.Data.WorldExposed as W
import qualified SDLHelper.Data.Rect as R
import qualified SDLHelper.SDLHelper as H

import LevelData

import qualified Player as P

import qualified Data.Map as Map

import GHC.Float (float2Int)

levels :: [Level]
levels = [
        Map.fromList [
            ((1,1), Wall),
            ((1,3), Box []),
            ((3,3), Box []),
            ((1,5), Hole),
            ((1,6), FilledHole)
        ]
    ]

playerStart = (0, 0)

renderLevel :: W.World -> Level -> IO ()
renderLevel w l = Map.foldrWithKey f (pure ()) l where
    f :: (Int, Int) -> Tile -> IO () -> IO ()
    f (x,y) tile io = H.renderSimple w (sprite tile) (scaledPos x y) >> io

    scaledPos :: Int -> Int -> SDL.V2 Int
    scaledPos x y = SDL.V2 (x*tw) (y*tw)

    sprite :: Tile -> MD.Sprite
    sprite t = case t of
        Box _ -> (W.tileSprites w) Map.! (Box [])
        _     -> (W.tileSprites w) Map.! t

    tw = W.tileWidth w

movePlayer :: W.World -> Dir -> W.World
movePlayer w d = case tileAhead of
    Nothing         -> updatePlayerPos w d
    Just FilledHole -> updatePlayerPos w d
    Just Wall       -> w
    Just Hole       -> w
    Just (Box _)    -> case maybeL' of
        Nothing -> w
        Just l' -> updateLevel (updatePlayerPos w d) l'
    where
        tileAhead = Map.lookup pos' l
        pos'      = move pos d
        pos       = getPlayerPos w
        l         = level w
        maybeL'   = moveBox l pos' (W.moveNum w) d
        updateLevel w' l' = w' { W.levels = setAt (W.levels w') (W.levelNum w') l'}

moveBox :: Level -> (Int,Int) -> Int -> Dir -> Maybe Level
moveBox l pos moveNum d = case tileAhead of
    Nothing         -> Just $ updateBoxPos l
    Just FilledHole -> Just $ updateBoxPos l
    Just Wall       -> Nothing
    Just Hole       -> Just $ fillHole l
    Just (Box _)    -> case maybeL' of
        Nothing -> Nothing
        Just l' -> Just $ updateBoxPos l'
    where
        tileAhead       = Map.lookup pos' l
        pos'            = move pos d
        updateBoxPos l' = updateKeyAndValue l' pos pos' updateBoxHistory 
        fillHole l'     = Map.adjust (const FilledHole) pos' (Map.delete pos l')
        maybeL'         = moveBox l pos' moveNum d 
        updateBoxHistory (Box s) = Box $ (moveNum, d):s

getPlayerPos :: W.World -> (Int, Int)
getPlayerPos w = mapTuple float2Int (R.rectX $ getPlayerRect w, R.rectY $ getPlayerRect w)

updatePlayerPos :: W.World -> Dir -> W.World
updatePlayerPos w d = w { W.player = p' } where
    p' = MD.changePos (W.player w) (mapTuple fromIntegral (getDirTup d))

getPlayerRect :: W.World -> R.Rect
getPlayerRect w' = P.rect (W.player w')

mapTuple :: (a->b) -> (a,a) -> (b,b)
mapTuple f (a,b) = (f a, f b)

move :: (Int, Int) -> Dir -> (Int, Int)
move pos d = addTup pos $ getDirTup d

getDirTup :: Dir -> (Int, Int)
getDirTup d = case d of
    Left  -> (-1,0)
    Right -> (1, 0)
    Up    -> (0,-1)
    Down  -> (0, 1)

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

level :: W.World -> Level
level w = W.levels w !! W.levelNum w

addTup :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTup (a,b) (c,d) = (a+c, b+d)

updateKey :: (Ord k) => Map.Map k a -> k -> k -> Map.Map k a
updateKey m k k' = Map.insert k' v m' where
    v = m Map.! k
    m' = Map.delete k m

updateKeyAndValue :: (Ord k) => Map.Map k a -> k -> k -> (a->a) -> Map.Map k a
updateKeyAndValue m k k' f = Map.insert k' (f v) m' where
    v = m Map.! k
    m' = Map.delete k m