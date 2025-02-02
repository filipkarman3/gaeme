module LevelData where
    
import qualified Data.Map as Map

data Dir  = Left | Right | Up | Down deriving (Eq, Ord, Show)
data Tile = Box [(Int, Dir)] | Wall | Hole | FilledHole deriving (Eq, Ord, Show)
data TileEntity = PlayerEnt | BoxEnt

type Level = Map.Map (Int, Int) Tile


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
