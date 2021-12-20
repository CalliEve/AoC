import Data.List
import Debug.Trace

getInput :: String -> IO (String, [String])
getInput path = do 
    contents <- readFile path >>= return . lines
    return (head contents, drop 2 contents)

solve :: Int -> String -> [String] -> Int
solve 0 _ m = sum $ map (length . filter (=='#')) m
solve i e m = solve (i - 1) e (lines $ enhanceMap i 0 0 e $ enlargeMap i m)

traceMap :: [String] -> [String]
traceMap xs = trace (unlines xs) xs

enlargeMap :: Int -> [String] -> [String]
enlargeMap i m 
    | even i = (replicate rowLength '.') : (map (\x -> '.' : x ++ ['.']) m) ++ [(replicate rowLength '.')]
    | otherwise = (replicate rowLength '#') : (map (\x -> '#' : x ++ ['#']) m) ++ [(replicate rowLength '#')]
    where rowLength = 2 + (length $ head m)

enhanceMap :: Int -> Int -> Int -> String -> [String] -> String
enhanceMap i x y e m 
    | (y + 1) >= columnLength && (x + 1) >= rowLength = [new]
    | (x + 1) >= rowLength = new : '\n' : enhanceMap i 0 (y + 1) e m
    | otherwise = new : enhanceMap i (x + 1) y e m
    where rowLength = length $ head m
          columnLength = length m
          new = getField e $ getNeighbors (if even i then '.' else '#') x y m

getNeighbors :: Char -> Int -> Int -> [String] -> String
getNeighbors c 0 0 m = replicate 7 c ++ [m !! 1 !! 0, m !! 1 !! 1]
getNeighbors c x 0 m 
    | (x + 1) >= rowLength = replicate 6 c ++ [m !! 1 !! (x - 1), m !! 1 !! x, c]
    | otherwise = replicate 6 c ++ [m !! 1 !! (x - 1), m !! 1 !! x, m !! 1 !! (x + 1)]
    where rowLength = length $ head m
getNeighbors c 0 y m 
    | (y + 1) >= columnLength = [c, m !! (y - 1) !! 0, m !! (y - 1) !! 1, c, m !! y !! 0, m !! y !! 1] ++ replicate 3 c
    | otherwise = [c, m !! (y - 1) !! 0, m !! (y - 1) !! 1, c, m !! y !! 0, m !! y !! 1, c, m !! (y + 1) !! 0, m !! (y + 1) !! 1]
    where columnLength = length m
getNeighbors c x y m 
    | (y + 1) >= columnLength && (x + 1) >= rowLength = [m !! (y - 1) !! (x - 1), m !! (y - 1) !! x] ++ replicate 7 c
    | (x + 1) >= rowLength = [m !! (y - 1) !! (x - 1), m !! (y - 1) !! x, c, m !! y !! (x - 1), m !! y !! x, c, m !! (y + 1) !! (x - 1), m !! (y + 1) !! x, c]
    | (y + 1) >= columnLength = [m !! (y - 1) !! (x - 1), m !! (y - 1) !! x, m !! (y - 1) !! (x + 1), m !! y !! (x - 1), m !! y !! x, m !! y !! (x + 1)] ++ replicate 3 c
    | otherwise = [m !! (y - 1) !! (x - 1), m !! (y - 1) !! x, m !! (y - 1) !! (x + 1), m !! y !! (x - 1), m !! y !! x, m !! y !! (x + 1), m !! (y + 1) !! (x - 1), m !! (y + 1) !! x, m !! (y + 1) !! (x + 1)]
    where rowLength = length $ head m
          columnLength = length m

getField :: String -> String -> Char
getField e s = e !! (parseBinary $ map (\x -> if x == '.' then '0' else '1') s)

parseBinary :: String -> Int
parseBinary [] = 0
parseBinary (x:xs) = (if x == '1' then 2^(length xs) else 0) + parseBinary xs

main = do
    (enhancer, map) <- getInput "./input.txt"
    print $ solve 50 enhancer map

