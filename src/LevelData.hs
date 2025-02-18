module LevelData where
    
import qualified Data.Map as Map

data Dir  = Left | Right | Up | Down deriving (Eq, Ord, Show, Enum)
data Tile = Box [(Int, Dir)] | Wall | Hole | FilledHole deriving (Eq, Ord, Show)
data TileEntity = PlayerEnt | BoxEnt

type Level = Map.Map (Int, Int) Tile

-- a series is a set of levels
-- String represents the name
-- Int represents the difficulty
-- (Int,Int) represents start position of first level
-- (Int,Int) (the second one) represents cheese pos in the final level
-- [(Int,Int)] represents the location of the cheese signs in each level
-- (start position of following levels depends on your y-co-ordinate when you screen transition)
data Series = Series {
        name :: String,
        difficulty :: Int,
        startPos :: (Int,Int),
        cheesePos :: (Int,Int),
        signPos :: [(Int,Int)],
        ls :: [Level],
        currentLevel :: Int,
        savedLevel   :: Level,
        savedPlayerPos :: (Int, Int)
    }

newSeries :: String -> Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [Level] -> Series
newSeries name' diff start cheese sign ls' = Series {
        name = name',
        difficulty = diff,
        startPos = start,
        cheesePos = cheese,
        signPos = sign,
        ls = ls',
        currentLevel = 0,
        savedLevel = ls'!!0,
        savedPlayerPos = start
    }

loadSeries :: Series -> Series
loadSeries s = s {
        currentLevel = 0,
        savedLevel = ls s !! 0,
        savedPlayerPos = startPos s
    }

dummySeries = newSeries "abce " 0 (0,0) (0,0) [] []

-- contains all the series in the game
-- Int is the index of the series that is being focused on whilst in the menu
data MenuData  = MenuData Int [Series]

initMenuData :: MenuData
initMenuData = MenuData 0 [s1, s2, s3]

s1 = newSeries "base" 1 (3,2) (11,4) sign ls1 where
    sign = [(10,5),(10,3),(13,6),(9,9),(13,5),(4,5)]
s2 = newSeries "box programming" 5 (0,11) (15,3) [(3,18)] ls2
s3 = newSeries "rob name this one" 2 (0,1) (19,8) [(3,18)] ls3

ls1 :: [Level]
ls1 = [
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
                (6,5),
                (6,6),
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
                (3,3)
            ]
        ),

        Map.fromList (changePos (0,4) (
            makeTiles Wall [
                (2,0), (3,0), (4,0), (5,0), (6,0), (7,0),
                (2,1), (7,1),
                (2,2), (5,2),
                (0,3), (1,3), (2,3), (3,3), (5,3),
                (5,4),
                (0,5), (1,5), (2,5), (5,5),
                (2,6), (3,6), (4,6), (5,6)
            ] ++ makeTiles (Box []) [
                (3,4),(4,3)
            ] ++ makeTiles Wall [
                (8,1),(9,1),(10,1),(11,1),(12,1),(13,1),(14,1),(15,1),(16,1),(17,1),(18,1),(19,1),
                (6,3),(7,3),(8,3),(9,3),(10,3),(11,3),(12,3),(13,3),(14,3),(15,3),(16,3),(17,3),(18,3),(19,3)
            ])
        ),

        Map.fromList (changePos (0,3) (
            makeTiles Wall [
                (0,1), (1,1), (2,1), (3,1), (4,1), (5,1), (6,1), (7,1), (8,1), (9,1),
                (0,2), (9,2),
                (2,3), (3,3), (4,3), (5,3), (6,3), (8,3), (9,3),
                (0,4), (9,4),
                (0,5), (1,5), (2,5), (5,5), (6,5), (8,5), (9,5),
                (6,6), (2,6),(3,6),(4,6),(5,6), 
                (6,7), (8,7), (9,7),
                (6,8), (7,8), (8,8)] ++ makeTiles (Box []) [
                    (7,2),(7,3),(7,4),(7,5),(6,4),(3,4)
                ] ++ makeTiles Wall [
                    (10,5),(11,5),(12,5),(13,5),(14,5),(15,5),(16,5),(17,5),(18,5),(19,5),
                    (10,7),(11,7),(12,7),(13,7),(14,7),(15,7),(16,7),(17,7),(18,7),(19,7)
                ])

        ),

        Map.fromList (changePos (0,8) (
            makeTiles Wall [
                (0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),
                (8,1),
                (0,2),(6,2),(7,2),(8,2),
                (0,3),(1,3),(2,3),(4,3),(8,3),
                (2,4),(6,4),
                (2,5),(3,5),(6,5),
                (3,6),(4,6),(5,6),(6,6)
            ] ++ makeTiles (Box []) [
                (2,1),(3,1),(4,1),(6,1),(5,2),(5,3),(5,4)
            ] ++ makeTiles Wall [
                (7,5),(8,5),(9,5),(10,5),(11,5),(12,5),(13,5),(14,5),
                (9,3),(10,3),(11,3),(12,3),
                (14,4),(14,3),(14,2),(14,1),(14,0),(14,-1),(14,-2),(14,-3),(14,-4),(14,-5),
                (14,-6),(15,-6),(16,-6),(17,-6),(18,-6),(19,-6),
                (12,2),(12,1),(12,0),(12,-1),(12,-2),(12,-3),(12,-4),(12,-5),(12,-6),(12,-7),(12,-8),
                (13,-8),(14,-8),(15,-8),(16,-8),(17,-8),(18,-8),(19,-8)
            ])
        ),

        Map.fromList (
            makeTiles Wall [
                (0,0),(1,0),(2,0),(3,0),(4,0),(5,0),
                (5,1),
                (0,2),(1,2),(3,2),(5,2),(6,2),(7,2),
                (0,3),(3,3),(7,3),
                (0,4),(3,4),(4,4),(5,4),(7,4),(8,4),(9,4),(10,4),(11,4),
                (0,5),(1,5),(11,5),
                (1,6),(3,6),(4,6),(5,6),(6,6),(8,6),(9,6),(10,6),(11,6),
                (1,7),(10,7),
                (1,8),(2,8),(3,8),(5,8),(6,8),(8,8),
                (3,9),(8,9),(9,9),
                (3,10),(4,10),(5,10),(9,10),
                (5,11),(8,10),(9,11),
                (5,12),(6,12),(7,12),(8,12),(9,12)

            ] ++ makeTiles (Box []) [
                (2,3),(3,5),(4,5),(5,5),(7,5),(8,5),(7,7),(7,8),(7,9),(7,10), (6,7),(5,7),(4,7),(3,7), (7,11)
            ] ++ makeTiles Wall [
                (11,7),(12,7),(13,7),(14,7),(15,7),(16,7),(17,7),(18,7),(19,7),
                (10,9),(11,9),(12,9),(13,9),(14,9),(15,9),(16,9),(17,9),(18,9),(19,9)
            ]
        ),

        Map.fromList (changePos (0,3) (
            makeTiles Wall [
                (9,0),(10,0),
                (8,1),(9,1),(13,1),
                (8,2),(11,2),(12,2),
                (8,3),(9,3),(11,3),
                (0,4),(1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,4),(8,4),(11,4),
                (11,5),
                (0,6),(2,6),(3,6),(4,6),(5,6),(6,6),(7,6),(8,6),(11,6),
                (0,7),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7),(8,7),(10,7),(11,7),
                (0,8),(2,8),(4,8),(8,8),(11,8),(12,8),
                (0,9),(6,9),(12,9),
                (0,10),(2,10),(3,10),(4,10),(5,10),(6,10),(7,10),(8,10),(9,10),(10,10),(12,10),
                (0,11),(12,11),
                (0,12),(1,12),(2,12),(3,12),(4,12),(5,12),(8,12),(9,12),(10,12),(11,12),(12,12),
                (5,13),(6,13),(7,13),(8,13),
                (8,6),(8,7),(8,8)
            ] ++ makeTiles (Box []) [
                (10,4),(9,5),(8,5),(9,6),(9,7),(9,8),(9,9),(2,9),(7,11)
            ]) ++ makeTiles Wall [
                (19,10),(19,12)
            ]
        )
    ]

ls2 :: [Level]
ls2 = [
        Map.fromList (changePos (0,0) (
            makeTiles Wall [
                                           ( 3, 0), ( 4, 0), ( 5, 0), ( 6, 0), ( 7, 0), ( 8, 0), ( 9, 0), (10, 0), (11, 0), (12, 0), (13, 0), (14, 0), (15, 0),
                                           ( 3, 1),                                              ( 9, 1),                                              (15, 1),
                                           ( 3, 2),          ( 5, 2),          ( 7, 2),          ( 9, 2),          (11, 2), (12, 2),          (14, 2), (15, 2),
                                           ( 3, 3),                   ( 6, 3), ( 7, 3),                            (11, 3), (12, 3),
                         ( 1, 4), ( 2, 4), ( 3, 4),          ( 5, 4), ( 6, 4), ( 7, 4), ( 8, 4),          (10, 4), (11, 4), (12, 4),          (14, 4), (15, 4),
                ( 0, 5), ( 1, 5),          ( 3, 5),                                     ( 8, 5),          (10, 5), (11, 5), (12, 5),                   (15, 5),
                ( 0, 6),                   ( 3, 6), ( 4, 6), ( 5, 6), ( 6, 6),          ( 8, 6),                            (12, 6), (13, 6),          (15, 6),
                ( 0, 7), ( 1, 7),          ( 3, 7),          ( 5, 7), ( 6, 7),          ( 8, 7),          (10, 7),                                     (15, 7),
                ( 0, 8),                                                                                  (10, 8), (11, 8), (12, 8), (13, 8), (14, 8), (15, 8),
                ( 0, 9), ( 1, 9),          ( 3, 9),          ( 5, 9), ( 6, 9), ( 7, 9), ( 8, 9), ( 9, 9), (10, 9),
                ( 0,10), ( 1,10), ( 2,10), ( 3,10),          ( 5,10),
                                                             ( 5,11),
                ( 0,12), ( 1,12), ( 2,12), ( 3,12), ( 4,12), ( 5,12)
            ] ++ makeTiles (Box []) [
                (4,10),(4,4),(5,1),(9,7),(9,6),(9,5),(9,4),(13,2),(13,3),(13,4)
            ]
        ))
    ]

ls3 :: [Level]
ls3 = [
        Map.fromList (
            makeTiles Wall [
                (0,0),(1,0),(2,0),(3,0),(4,0),(5,0),
                (3,1),(5,1),
                (0,2),(1,2),(3,2),(5,2),(6,2),(7,2),
                (0,3),(3,3),(7,3),
                (0,4),(3,4),(4,4),(5,4),(6,4),(7,4),(8,4),(9,4),(10,4),(11,4),
                (0,5),(3,5),(4,5),(11,5),
                (0,6),(3,6),(4,6),(5,6),(6,6),(8,6),(9,6),(10,6),(11,6),
                (0,7),
                (0,8),(1,8),(2,8),(3,8),(8,8),(9,8),
                (3,9),(4,9),(5,9),(8,9),(9,9),
                (5,10),(9,10),
                (5,11),(8,10),(9,11),
                (5,12),(6,12),(7,12),(8,12),(9,12)
            ] ++ makeTiles (Box []) [
                --(2,3),(3,5),(4,5),(5,5),(7,5),(8,5),
                (7,7),(7,8),(7,9),(7,10), (6,7),(5,7),(4,7),(3,7),(2,7),(7,11)
            ] ++ makeTiles Wall [
                (11,7),(12,7),(13,7),(14,7),(15,7),(16,7),(17,7),(18,7),(19,7),
                (10,9),(11,9),(12,9),(13,9),(14,9),(15,9),(16,9),(17,9),(18,9),(19,9)
            ]
        )
    ]

makeTiles :: Tile -> [(Int,Int)] -> [((Int,Int), Tile)]
makeTiles t xs = map (\x -> (x,t)) xs

changePos :: (Int,Int) -> [((Int,Int),Tile)] -> [((Int,Int),Tile)]
changePos (x,y) = map (\((x',y'),t) -> ((x'+x,y'+y),t))