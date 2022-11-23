import Data.List

parseIntList :: [String] -> [Int]
parseIntList [] = []
parseIntList (x:xs) = (read x :: Int) : parseIntList xs

getListInput :: String -> IO [String]
getListInput path = do
    content <- readFile path
    return $ lines content

split :: Char -> String -> [String]
split c str = case break (==c) str of
                ("", c:b) -> split c b
                (a, c:b) -> a : split c b
                (a, "")    -> [a]

parseBoards :: [String] -> [[[(Int, Bool)]]]
parseBoards [] = []
parseBoards (_:a:b:c:d:e:xs) = createBoard (a:b:c:d:e:[]) : parseBoards xs

createBoard :: [String] -> [[(Int, Bool)]]
createBoard [] = []
createBoard (x:xs) = (map (\x -> (x, False)) $ parseIntList $ split ' ' x) : createBoard xs

boardHasWonRow :: [[(Int, Bool)]] -> Bool
boardHasWonRow [] = False
boardHasWonRow (r:rs) = (and $ map (\(_,b) -> b) r) || boardHasWonRow rs

boardHasWonColumn :: [[(Int, Bool)]] -> Bool
boardHasWonColumn b = boardHasWonRow $ transpose b

boardHasWon :: [[(Int, Bool)]] -> Bool
boardHasWon b = boardHasWonRow b || boardHasWonColumn b

calculateScore :: [[(Int, Bool)]] -> Int
calculateScore rs = sum $ map (\r -> sum $ map (\(x,t) -> if not t then x else 0) r) rs

updateBoards :: [[[(Int, Bool)]]] -> Int -> [[[(Int, Bool)]]]
updateBoards bs x = map (\b -> map (\r -> map (\(y,t) -> (y,if t then t else y == x)) r) b) bs

solve :: [[[(Int, Bool)]]] -> [Int] -> Int
solve bs [] = error "no winner found"
solve (b:[]) (x:xs) = 
    if boardHasWon $ head updated
    then (calculateScore $ head $ updated) * x
    else solve updated xs
    where
        updated = updateBoards (b:[]) x
solve bs (x:xs) = solve (filter (\b -> not $ boardHasWon b) $ updateBoards bs x) xs

main = do
    input <- getListInput "./input.txt"
    let numbers = parseIntList $ split ',' $ head input
    let boards = parseBoards $ tail input
    print $ solve boards numbers 

