module AOCUtil (module AOCUtil) where

import Data.List ( group, sort, sortOn )
import Data.Ord ( Down(Down) )

interactF :: String -> (String -> String) -> IO ()
interactF file fun = (putStrLn . fun) =<< readFile file

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn p s = case dropWhile (p ==) s of
  [] -> []
  s' -> w : splitOn p s'' where (w, s'') = break (== p) s'

impl :: Bool -> Bool -> Bool
impl a b = not a || b

unDigits :: [Int] -> Int
unDigits [] = 0
unDigits [i] = i
unDigits is = unDigits (init is) * 10 + last is

middle :: [a] -> a
middle [] = error "called AOCUtil.middle on empty list."
middle ls = ls !! (length ls `div` 2)

mapFst :: (a->c) -> (a,b) -> (c,b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b->c) -> (a,b) -> (a,c)
mapSnd f (a, b) = (a, f b)

readBin :: String -> Int
readBin [] = 0
readBin ls = readBin (init ls) * 2 + toBin (last ls)
  where
    toBin '0' = 0
    toBin '1' = 1
    toBin c = error $ "Non binary digit: " ++ [c]

first2 :: [a] -> (a, a)
first2 (a : b : _) = (a, b)
first2 ls = error $ "list to short for first2, len: " ++ show (length ls)

first3 :: [a] -> (a, a, a)
first3 (a : b : c : _) = (a, b, c)
first3 ls = error $ "list to short for first3, len: " ++ show (length ls)

asList2 :: (a, a) -> [a]
asList2 (a, b) = [a, b]

asList3 :: (a, a, a) -> [a]
asList3 (a, b, c) = [a, b, c]

in2 :: Eq a => a -> (a, a) -> Bool
in2 a = (a `elem`) . asList2

in3 :: Eq a => a -> (a, a, a) -> Bool
in3 a = (a `elem`) . asList3

mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f (a1, a2) = (f a1, f a2)

mapT3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapT3 f (a1, a2, a3) = (f a1, f a2, f a3)

applyT :: (a -> b, a -> c) -> a -> (b, c)
applyT (f1, f2) a = (f1 a, f2 a)

mostCommon :: Ord a => [a] -> a
mostCommon = head . head . sortOn (Down . length) . group . sort

without :: Int -> [a] -> [a]
without _ [] = []
without 0 (_ : as) = as
without i (a : as) = a : without (i - 1) as

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) v = min u $ max l v

rotateR :: Int -> [a] -> [a]
rotateR 0 ls = ls
rotateR i ls
  | i < 0 = rotateL (-i) ls
  | otherwise = rotateR (i - 1) (last ls : init ls)

rotateL :: Int -> [a] -> [a]
rotateL 0 ls = ls
rotateL i ls
  | i < 0 = rotateR (-i) ls
  | otherwise = rotateL (i - 1) (tail ls ++ [head ls])

readChar :: Char -> Int
readChar c = fromEnum c - fromEnum '0'

addT :: Num a => (a, a) -> (a, a) -> (a, a)
addT (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

manhattan :: Num a => (a, a) -> (a, a) -> a
manhattan (a1, a2) (b1, b2) = abs (a1 - b1) + abs (a2 - b2)

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a
