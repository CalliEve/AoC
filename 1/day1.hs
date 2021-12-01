parseIntList :: [String] -> [Int]
parseIntList [] = []
parseIntList (x:xs) = (read x :: Int) : parseIntList xs

getIntListInput :: String -> IO [Int]
getIntListInput path = do
    content <- readFile path
    return $ parseIntList $ lines content

increasesCount :: [Int] -> Int
increasesCount (x:y:z:[]) = 0
increasesCount (x:y:z:xs) = if (x + y + z) < (y + z + head xs)
    then 1 + increasesCount (y:z:xs)
    else increasesCount (y:z:xs)

main = do
    input <- getIntListInput "./input.txt"
    print $ increasesCount $ input

