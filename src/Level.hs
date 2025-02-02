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

import qualified Data.Either as Either
import Data.Maybe

levels :: [Level]
levels = [
        Map.fromList (
            makeTiles Wall [
                (2,1),
                (3,1),
                (4,1),
                (1,2),
                (2,2),
                (1,3),
                (1,4),
                (1,5),
                (2,5),
                (3,5),
                (4,5),
                (4,6),
                (4,7),
                (4,8),
                (4,9),
                (6,9),
                (7,9),
                (4,2),
                (4,3),
                (5,3),
                (6,3),
                (6,4),
                (7,4),
                (7,5),
                (7,6),
                (7,7),
                (6,7),
                (5,9),
                (6,9),
                (7,9),
                (8,9),
                (9,9),
                (10,9),
                (11,9),
                (12,9),
                (13,9),
                (14,9),
                (15,9),
                (16,9),
                (17,9),
                (18,9),
                (19,9),
                (20,9),
                (8,7),
                (9,7),
                (10,7),
                (11,7),
                (12,7),
                (13,7),
                (14,7),
                (15,7),
                (16,7),
                (17,7),
                (18,7),
                (19,7),
                (20,7)


            ] ++ makeTiles (Box []) [
                (3,3),
                (5,5)
            ]
        ),

        Map.fromList (
            makeTiles (Box []) [
                (0,0), (10,10)
            ]
        )
    ]

makeTiles :: Tile -> [(Int,Int)] -> [((Int,Int), Tile)]
makeTiles t xs = map (\x -> (x,t)) xs

playerStart :: (Int,Int)
playerStart = (3, 2)

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
        updateLevel w' l' = w' {
            W.levels = setAt (W.levels w') (W.levelNum w') l',
            W.moveNum = W.moveNum w' + 1
        }

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
        updateBoxHistory (Box s) = Box $ (moveNum, revDir d):s


rewind :: W.World -> W.World
rewind w = rewind' (Map.toList l) moveNum
    where
        rewind' [] _ = toWorld w l
        rewind' ((pos,tile):xs) moveNum = case tile of
            Box ((i,d):_) -> if i == moveNum then rewind (toWorld w (fromEither (moveBoxReverse l pos moveNum d playerPos))) else rewind' xs moveNum
            _ -> rewind' xs moveNum
        fromEither (Either.Left l') = l'
        fromEither (Either.Right l') = l'
        toWorld w' l = w' {W.levels = setAt (W.levels w') (W.levelNum w') l}

        moveNum = W.moveNum w
        l = W.levels w !! W.levelNum w
        playerPos = getPlayerPos w

-- Should be blocked by player as well
moveBoxReverse :: Level -> (Int,Int) -> Int -> Dir -> (Int,Int) -> Either Level Level
moveBoxReverse l pos moveNum d playerPos = if pos' == playerPos then Either.Left (Map.adjust (const updatedBox) pos l)
    else case tileAhead of
    Nothing -> Either.Right $ updateBoxPos l
    Just FilledHole -> Either.Right $ updateBoxPos l
    Just Wall -> Either.Left $ Map.adjust (const updatedBox) pos l
    Just Hole -> Either.Right $ fillHole l
    Just (Box _) -> case eitherL' of
        Either.Left l' -> Either.Left $ Map.adjust (const updatedBox) pos l'
        Either.Right l' -> Either.Right $ updateBoxPos l'
    where
        (Box s) = fromJust $ Map.lookup pos l
        tileAhead = Map.lookup pos' l
        pos' = move pos d
        updateBoxPos l' = updateKeyAndValue l' pos pos' (const updatedBox)
        fillHole l' = Map.adjust (const FilledHole) pos' (Map.delete pos l')
        eitherL' = moveBoxReverse l pos' moveNum d playerPos
        updatedBox = case s of
            ((i,x):xs) -> if i == moveNum then Box xs else Box ((i,x):xs)
            _ -> Box s

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

revDir :: Dir -> Dir
revDir Left = Right
revDir Right = Left
revDir Up = Down
revDir Down = Up

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