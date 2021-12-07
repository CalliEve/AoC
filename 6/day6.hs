-- Parse the input list to a list of 9 integers depicting all possible states of the lantern fish
getListInput :: String -> IO [Int]
getListInput path = do
    content <- readFile path
    return $ groupBy [0,0,0,0,0,0,0,0,0] $ map read $ words $ map f $ head $ lines content
    where f ',' = ' '
          f x = x

-- add a value `val` to position `pos` in the list xs
addToPos :: Int -> Int -> [Int] -> [Int]
addToPos pos val xs = (s ++ [n]) ++ tail e
    where (s, e) = splitAt pos xs
          n = head e + val

-- group the input list to the list with states by counting how many of each state there are
groupBy :: [Int] -> [Int] -> [Int]
groupBy all [] = all
groupBy all (x:xs) = groupBy (addToPos x 1 all) xs

-- solve the challenge by growing the school of lantern fish each day,
-- till at the end summing them up to get the total of lantern fish
solve :: [Int] -> Int -> Int
solve xs 0 = sum xs
solve xs i = solve (grow xs) (i - 1)

-- grow the school of lantern fish by grabbing the head of the list and
-- adding it to pos 6 in the tail of the list, before adding the zero back at the end of the list
grow :: [Int] -> [Int]
grow all = updated
    where zero = head all
          updated = addToPos 6 zero $ tail all ++ [zero]

main = do
    input <- getListInput "./input.txt"
    print $ solve input 256

