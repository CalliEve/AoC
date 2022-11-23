import Data.List
import Data.Maybe

getListInput :: String -> IO [String]
getListInput path = readFile path >>= return . lines

solve :: [String] -> [Maybe Int]
solve [] = []
solve (x:xs) = track x [] : solve xs

track :: String -> [Char] -> Maybe Int
track [] st = Just $ calcPoints $ reverse st
track ('{':xs) st = track xs ('}':st)
track ('(':xs) st = track xs (')':st)
track ('[':xs) st = track xs (']':st)
track ('<':xs) st = track xs ('>':st)
track (x:xs) (s:st) = if x == s then track xs st else Nothing

calcPoints :: [Char] -> Int
calcPoints [] = 0
calcPoints (')':st) = 1 + 5 * calcPoints st
calcPoints (']':st) = 2 + 5 * calcPoints st
calcPoints ('}':st) = 3 + 5 * calcPoints st
calcPoints ('>':st) = 4 + 5 * calcPoints st

getMiddleVal :: [a] -> a
getMiddleVal xs = xs !! (length xs `div` 2)

main = getListInput "./input.txt" >>= print . getMiddleVal . sort . catMaybes . solve

