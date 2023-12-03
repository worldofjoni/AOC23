{-# LANGUAGE TupleSections #-}
module Days.Day03(module Days.Day03) where

import Data.Char (isDigit)
import Data.List (groupBy)
import Data.List.Extra ((!?))
import Data.Maybe (catMaybes)
import AOCUtil (interactF)
import qualified Data.HashMap as Hash (Map, insertWith, empty, filter, toList)


runA :: IO ()
runA = interactF "data/day03.txt" solveA
runB :: IO ()
runB = interactF "data/day03.txt" solveB

solveA :: String -> String
solveA s = show . sum . map (readNum m) . filter (any special . adjacents m) $ nums where
    m = lines s
    nums = findNums m

solveB :: String -> String
solveB s = show . sum .  map (product . map (readNum m) . snd) . Hash.toList . Hash.filter ((==2). length) . foldl (\hm (n, g) -> Hash.insertWith (++) g [n] hm ) Hash.empty . concatMap (\n -> [(n, g) | g <- adjacentGears m n]) $ nums where
    m = lines s
    nums = findNums m


type Pos = (Int, Int)
type Numb = [Pos]
type Schematic = [String]

type Gears = Hash.Map Pos [Numb]

findNums :: Schematic -> [Numb]
findNums = concatMap (uncurry findNumsInLine) . zip [0..]


findNumsInLine :: Int -> String -> [Numb]
findNumsInLine i = groupAdjacent fst . map ((,i).fst) . filter (isDigit . snd) . zip [0..] where



groupAdjacent :: (a -> Int) -> [a] -> [[a]]
groupAdjacent f = map (map snd) . groupBy (\(i,a) (j,b) -> i - f a == j - f b) . zip [0..]

adjacents :: Schematic -> Numb -> [Char]
adjacents m poss = catMaybes [m !? y >>= (!? x) | (x,y) <- left ++ right ++ above ++ below] where
    left = let (x,y) = head poss in [(x-1, y), (x-1, y-1), (x-1, y+1)]
    right = let (x,y) = last poss in [(x+1, y), (x+1, y-1), (x+1, y+1)]
    above = map (\(x,y)-> (x,y-1)) poss
    below = map (\(x,y)-> (x,y+1)) poss

readNum :: Schematic -> Numb -> Int
readNum m num = read [m !! y !! x | (x,y) <- num]

special :: Char -> Bool
special c = not (isDigit c) && c /= '.'

gear :: Char -> Bool
gear c = c == '*'

adjacentGears :: Schematic -> Numb -> [Pos]
adjacentGears m poss = filter (\(x,y) -> any gear $ m !? y >>= (!? x)) (left ++ right ++ above ++ below) where
    left = let (x,y) = head poss in [(x-1, y), (x-1, y-1), (x-1, y+1)]
    right = let (x,y) = last poss in [(x+1, y), (x+1, y-1), (x+1, y+1)]
    above = map (\(x,y)-> (x,y-1)) poss
    below = map (\(x,y)-> (x,y+1)) poss