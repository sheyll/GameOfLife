module GameOfLife (Pos, Board, update_life, example_boards, randPos, randBoard, randomElem, creatures, stones, translateBrd, emptyBoard, joinBoards, toList, boardSize, swidth, sheight, sdepth) where

import System.Random
import qualified Data.Set as Set
import Control.Applicative

type Pos = (Int, Int, Int)
type Board = Set.Set Pos

emptyBoard :: Board
emptyBoard = Set.empty

toList :: Board -> [Pos]
toList = Set.toList

boardSize :: Board -> Int
boardSize = Set.size

joinBoards :: Board -> Board -> Board
joinBoards = Set.union

update_life :: Board -> (Board, Board, Board, Board)
update_life b = let (s, d) = survivors_and_deaths b
                    c = births b
                    nextgen = s `Set.union` c
                in (c, d, s, nextgen)

creatures = map toBoard [c5ship, fish, fish2, glider]

example_boards = creatures ++ stones

swidth = 100
sheight = 100
sdepth = 100
sdh = 50

randPos :: IO Pos
randPos = do x <- getStdRandom (randomR (0, swidth))
             y <- getStdRandom (randomR (0, sheight))
             z <- getStdRandom (randomR (0, sdepth))
             return (x,y,z)

randomElem :: [a] -> IO a
randomElem xs = do ri <- getStdRandom (randomR (0, (length xs) - 1))
                   return $ xs !! ri

randBoard :: [Board] -> Int -> IO Board
randBoard _ 0 = return Set.empty
randBoard boards (n + 1) = 
    do p <- randPos
       r <- getStdRandom (randomR (0, 3))
       o <- randomElem boards
       b <- randBoard boards n
       return $ (p `translateBrd` (r `rotate` o)) `Set.union`  b

-- internal functions
isAlive :: Board -> Pos -> Bool
isAlive b p = p `Set.member` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = p `Set.notMember` b

neighbs :: Pos -> [Pos]
neighbs (x,y,z) = [(x', y', z') | 
                    x' <- around swidth x, 
                    y' <- around sheight y, 
                    z' <- around sdepth z, 
                    (x', y', z') /= (x,y,z)]
    where around w = (wrap w <$>) . (<$> [-1 .. 1]) . (+)
          wrap w x | x < 0     = wrap w (x + w)
                   | x >= w    = wrap w (x - w)
                   | otherwise = x

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors_and_deaths :: Board -> (Board, Board)
survivors_and_deaths b = Set.partition (\p -> let lns = (liveneighbs b p) in lns >= 2 && lns <= 3) b

births :: Board -> Board
births b = let ns = let ns' = Set.fold (\p a -> foldr Set.insert a (neighbs p)) Set.empty b 
                    in ns' `Set.difference` b
           in Set.filter ((==4) . (liveneighbs b)) ns

rmdups :: (Eq a, Ord a) => [a] -> [a]
rmdups = Set.toList . Set.fromList
--rmdups [] = []
--rmdups (x:xs) = (x : (rmdups $! (filter (/= x) xs)))

rotate :: Int -> Board -> Board
rotate 0 b = b
rotate (n + 1) b = Set.map (\(y,x,z) -> (x,y,z)) (rotate n b)

translateBrd :: Pos -> Board -> Board
translateBrd (ox, oy, oz) xs = Set.map (\(x, y, z) -> (ox + x, oy + y, oz + z)) xs

toBoard :: [String] -> Board
toBoard xs = Set.fromList [(x, y, sdh) | (cs, y) <- zip xs [1..], (c, x) <- zip cs [1..], c /= ' ']

glider = ["O",
          "O O",
          "OO"]

stones = map toBoard 
         [[" O",
           "O O",
           "O O",
           " O" ],
         
          [" OO",
           "O  O",
           "O  O",
           " OO" ],

          [" O",
           "O O",
           "O  O",
           " OO" ],

          ["OO",
           "OO"]]

fish = [" OO",
        "OOOO",
        "OO OO",
        "  OO"]

fish2 = ["OO   00",
         "  OO   OO",
         "  OO   OO",
         "OO   OO"]

c5ship = ["    O     OO     OO     O ",
          "  O  OO OO  O   O  OO OO  O",
          "OOO    O O   O O   O O    OOO",
          "OO  O  OOO OO   OO OOO  O  OO",
          "OOO  OOO    O   O    OOO  OOO",
          " O O  O               O  O O",
          "  O                       O"]



