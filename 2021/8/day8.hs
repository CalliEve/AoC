import Data.List
import Data.Maybe

data InputPart = Pattern |
                 Output

getListInput :: String -> InputPart -> IO [[String]]
getListInput path Output = readFile path >>= return . map (\x -> map sort x) . map (\x -> words $ tail $ dropWhile (/='|') x) . lines
getListInput path Pattern = readFile path >>= return . map (\x -> map sort x) . map (\x -> words $ takeWhile (/='|') x) . lines

getPattern :: [(String, Int)] -> Int -> String
getPattern xs i = fst $ fromMaybe ("", -1) $ find (\x -> snd x == i) xs

matchPattern :: [(String, Int)] -> String -> Int
matchPattern xs i = snd $ fromMaybe ("", -1) $ find (\x -> fst x == i) xs

findPatterns :: [(String, Int)] -> Int -> [(String, Int)]
findPatterns pats 1 = findPatterns (map (\x -> if (length $ fst x) == 2 then (fst x, 1) else x) pats) 7
findPatterns pats 7 = findPatterns (map (\x -> if (length $ fst x) == 3 then (fst x, 7) else x) pats) 4
findPatterns pats 4 = findPatterns (map (\x -> if (length $ fst x) == 4 then (fst x, 4) else x) pats) 8
findPatterns pats 8 = findPatterns (map (\x -> if (length $ fst x) == 7 then (fst x, 8) else x) pats) 9
findPatterns pats 9 = findPatterns (map (\x -> if pat4 `intersect` fst x == pat4 && snd x == (-1) then (fst x, 9) else x) pats) 6
    where pat4 = getPattern pats 4
findPatterns pats 6 = findPatterns (map (\x -> if pat1 `intersect` fst x /= pat1 && (length $ fst x) == 6 then (fst x, 6) else x) pats) 0
    where pat1 = getPattern pats 1
findPatterns pats 0 = findPatterns (map (\x -> if (length $ fst x) == 6 && snd x == (-1) then (fst x, 0) else x) pats) 5
findPatterns pats 5 = findPatterns (map (\x -> if (fst x) `intersect` pat6 == fst x && snd x == (-1) then (fst x, 5) else x) pats) 3
    where pat6 = getPattern pats 6
findPatterns pats 3 = findPatterns (map (\x -> if (fst x) `intersect` pat9 == fst x && snd x == (-1) then (fst x, 3) else x) pats) 2
    where pat9 = getPattern pats 9
findPatterns pats 2 = map (\x -> if snd x == (-1) then (fst x, 2) else x) pats

translate :: [(String, Int)] -> [String] -> [Int]
translate _ [] = []
translate pats (x:xs) = matchPattern pats x : translate pats xs

listToInt :: [Int] -> Int
listToInt xs = read $ concat $ map show xs

main = do
    rawPats <- getListInput "./input.txt" Pattern
    output <- getListInput "./input.txt" Output
    let all = zip rawPats output
    print $ sum $ map listToInt $ map (\x -> translate (fst x) (snd x)) $ map (\x -> (findPatterns (map (\y -> (y, (-1))) $ fst x) 1, snd x)) all

