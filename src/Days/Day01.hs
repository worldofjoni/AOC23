module Days.Day01(module Days.Day01) where

import Data.Char (isDigit)
import AOCUtil

runA :: IO ()
runA = interactF "data/day01.txt" solveA
runB :: IO ()
runB = interactF "data/day01.txt" solveB

solveA :: String -> String
solveA = show . sum . map extract . lines

extract :: String -> Int
extract s = read [head digs, last digs] where
    digs = filter isDigit s

solveB :: String -> String
solveB = show . sum . map (concDig . reduceDigits) . lines where
    concDig ls = read (show (head ls) ++ show (last ls)) :: Int

reduceDigits :: String -> [Int]
reduceDigits [] = []
-- reduceDigits ('z':'e':'r':'o':ls)       = 0: reduceDigits ls
reduceDigits ls@('o':'n':'e':_)           = 1: reduceDigits (tail ls)
reduceDigits ls@('t':'w':'o':_)           = 2: reduceDigits (tail ls)
reduceDigits ls@('t':'h':'r':'e':'e':_)   = 3: reduceDigits (tail ls)
reduceDigits ls@('f':'o':'u':'r':_)       = 4: reduceDigits (tail ls)
reduceDigits ls@('f':'i':'v':'e':_)       = 5: reduceDigits (tail ls)
reduceDigits ls@('s':'i':'x':_)           = 6: reduceDigits (tail ls)
reduceDigits ls@('s':'e':'v':'e':'n':_)   = 7: reduceDigits (tail ls)
reduceDigits ls@('e':'i':'g':'h':'t':_)   = 8: reduceDigits (tail ls)
reduceDigits ls@('n':'i':'n':'e':_)       = 9: reduceDigits (tail ls)
reduceDigits (l:ls) = if isDigit l then read [l]: reduceDigits ls else reduceDigits ls