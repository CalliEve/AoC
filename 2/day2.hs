import Data.List

getListInput :: String -> IO [String]
getListInput path = do
    content <- readFile path
    return $ lines content

navigate :: String -> (Int, Int, Int) -> (Int, Int, Int)
navigate course (aim, d, h)
    | Just amount <- stripPrefix "up " course = (aim - read amount, d, h)
    | Just amount <- stripPrefix "down " course = (aim + read amount, d, h)
    | Just amount <- stripPrefix "forward " course = 
        let a = read amount
        in (aim, d + (aim * a), h + a)

processNavigationList :: [String] -> (Int, Int, Int) -> Int
processNavigationList [] (_, d, h) = d * h
processNavigationList (x:xs) nav = processNavigationList xs $ navigate x nav

main = do
    input <- getListInput "./input.txt"
    print $ processNavigationList input (0, 0, 0)

