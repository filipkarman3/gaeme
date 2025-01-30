module LevelData where
    
import qualified Data.Map as Map

data Dir  = Left | Right | Up | Down deriving (Eq, Ord, Show)
data Tile = Box [(Int, Dir)] | Wall | Hole | FilledHole deriving (Eq, Ord, Show)
data TileEntity = PlayerEnt | BoxEnt

-- instance Eq Tile where
--     a == b = case (a,b) of
--         (Box s1, Box s2) -> s1 == s2
--         (Wall, Wall)     -> True
--         (Hole, Hole)     -> True
--         (FilledHole, FilledHole) -> True
--         _ -> False

-- instance Ord Tile where

type Level = Map.Map (Int, Int) Tile
