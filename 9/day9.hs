import Data.List

getListInput :: String -> IO [[Int]]
getListInput path = readFile path >>= return . map (map read) . map (map (\x -> [x])) . lines

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

getBasin :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [[Int]] -> [(Int, Int, Int)]
getBasin [] all _ = all
getBasin ((v, x, y):xs) all m = getBasin st ((v, x, y):all) m
    where st = (filter (\z -> notElem z xs && notElem z all) $ filter (\(v, _, _) -> v /= 9) $ getNeighbors x y m) ++ xs

getNeighbors :: Int -> Int -> [[Int]] -> [(Int, Int, Int)]
getNeighbors x 0 xs = [l, r, b]
        where xr = xs !! 0
              yr = xs !! 1
              l = if x == 0 then (9, x + 1, 0) else (xr !! (x - 1), x - 1, 0)
              r = if (length xr - 1) == x then (9, x + 1, 0) else (xr !! (x + 1), x + 1, 0)
              b = (yr !! x, x, 1)
getNeighbors x y xs | length xs == y + 1 = [l, r, a]
        where xr = xs !! y
              zr = xs !! (y - 1)
              l = if x == 0 then (9, x - 1, y) else (xr !! (x - 1), x - 1, y)
              r = if (length xr - 1) == x then (9, x + 1, y) else (xr !! (x + 1), x + 1, y)
              a = (zr !! x, x, y - 1)
getNeighbors x y xs = [l, r, b, a]
        where xr = xs !! y
              yr = xs !! (y + 1)
              zr = xs !! (y - 1)
              l = if x == 0 then (9, x + 1, y) else (xr !! (x - 1), x - 1, y)
              r = if (length xr - 1) == x then (9, x + 1, y) else (xr !! (x + 1), x + 1, y)
              b = (yr !! x, x, y + 1)
              a = (zr !! x, x, y - 1)

inBasin :: (Int, Int, Int) -> [[(Int, Int, Int)]] -> Bool
inBasin _ [] = False
inBasin p (b:bs) = elem p b || inBasin p bs

gatherBasins :: Int -> Int -> [[(Int, Int, Int)]] -> [[Int]] -> [[(Int, Int, Int)]]
gatherBasins x y bs m | y >= length m - 1 && x >= length (m !! y) = bs
gatherBasins x y bs m | x >= length (m !! y) = gatherBasins 0 (y + 1) bs m
gatherBasins x y bs m = gatherBasins (x + 1) y updated m
    where v = m !! y !! x
          b = if v == 9 || inBasin (v, x, y) bs then [] else [getBasin [(v, x, y)] [] m]
          updated = b ++ bs

removeLowest :: [Int] -> [Int]
removeLowest (_:[]) = []
removeLowest (x:xs)
    | any (<=x) xs = x : removeLowest xs
    | otherwise = xs

max3 :: [Int] -> [Int] -> [Int]
max3 ms [] = ms
max3 ms (x:xs)
    | length ms < 3 = max3 (x:ms) xs
    | all (>x) ms = max3 ms xs
    | otherwise = max3 (x:(removeLowest ms)) xs

main = getListInput "./input.txt" >>= print . product . max3 [] . map length . gatherBasins 0 0 []

