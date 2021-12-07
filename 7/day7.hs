import GHC.Float (int2Float, float2Int)

-- parse input to a list of integers,
-- by taking the first line of the file, replacing all , to spaces,
-- splitting on spaces using words and then mapping all the words to integers
getListInput :: String -> IO [Int]
getListInput path = do
    content <- readFile path
    return $ map read $ words $ map f $ head $ lines content
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

main = do
    input <- getListInput "./input.txt"
    print $ calcFuel input $ calcMean input
