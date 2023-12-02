{-# LANGUAGE TupleSections #-}
module Days.Day02(module Days.Day02) where
import AOCUtil (splitOn, interactF)
import Data.List (isInfixOf)
import Data.Char (isDigit)

runA :: IO ()
runA = interactF "data/day02.txt" solveA
runB :: IO ()
runB = interactF "data/day02.txt" solveB

solveA :: String -> String
solveA = show . sum . map fst . filter (\(_, rs) -> all possible rs) . map parseGame . lines

solveB :: String -> String
solveB = show . sum . map (power . foldl maxRound (0,0,0) . snd . parseGame) . lines

type Game = (Int, [Round]) -- id, ...
type Round = (Int, Int, Int) -- red, green, blue

addRound :: Round -> Round -> Round
addRound (a,b,c) (d,e,f) = (a+d, b+e, c+f)

maxRound :: Round -> Round -> Round
maxRound (a,b,c) (d,e,f) = (max a d, max b e, max c f)

power :: Round -> Int
power (a,b,c) = a * b * c

parseRound :: String -> Round
parseRound = foldl addRound (0,0,0) . map singleRound . splitOn ',' where
    singleRound :: String -> Round
    singleRound s
        | "red" `isInfixOf` s = (,0,0) . read . filter isDigit $ s
        | "green" `isInfixOf` s = (0, ,0) . read . filter isDigit $ s
        | "blue" `isInfixOf` s = (0,0,) . read . filter isDigit $ s
        | otherwise = error ("not valid: " ++ s)

parseGame :: String -> Game
parseGame s = (ident, rounds) where
    ident = read . filter isDigit . head . splitOn ':' $ s
    rounds =  map parseRound . splitOn ';' . head .  tail . splitOn ':' $ s

possible :: Round -> Bool
possible (r,g,b) = r <= 12 && g <= 13 && b <= 14
