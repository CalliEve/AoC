import GHC.Float (int2Float, float2Int)

-- parse input to a list of integers,
-- by taking the first line of the file, replacing all , to spaces,
-- splitting on spaces using words and then mapping all the words to integers
getListInput :: String -> IO [Int]
getListInput path = readFile path >>= return . map read . words . map f . head . lines
    where f ',' = ' '
          f x = x

-- calculate the total fuel usage for a given position and the list of crab submarines 
calcFuel :: [Int] -> Int -> Int
calcFuel [] _ = 0
calcFuel (x:xs) pos = calcDistFuel (a - b) + calcFuel xs pos
    where a = max x pos
          b = min x pos

-- use a summation to calculate the fuel usage for a given distance
calcDistFuel :: Int -> Int
calcDistFuel n = (n * (n + 1)) `div` 2

-- calculate mean of a list
calcMean :: [Int] -> Int
calcMean xs = sum xs `div` length xs

-- calculate the minimum fuel used by taking the mean and then looking at the 3 integers surrounding it
calcMinimumFuel :: [Int] -> Int
calcMinimumFuel xs = minimum [calcFuel xs $ m - 1, calcFuel xs m, calcFuel xs $ m + 1]
    where m = calcMean xs

main = getListInput "./input.txt" >>= print . calcMinimumFuel

