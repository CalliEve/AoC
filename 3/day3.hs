getIntListInput :: String -> IO [String]
getIntListInput path = do
    content <- readFile path
    return $ lines content

parseBinary :: String -> Int
parseBinary [] = 0
parseBinary (x:xs) = (if x == '1' then 2^(length xs) else 0) + parseBinary xs

addPos :: Bool -> (Int, Int) -> (Int, Int)
addPos True (z, o) = (z, o + 1)
addPos False (z, o) = (z + 1, o)

howCommon :: [String] -> Int -> (Int, Int) -> (Int, Int)
howCommon [] _ c = c 
howCommon (x:xs) pos c = howCommon xs pos (addPos ((x !! pos) == '1') c)

data Setting = Most
             | Least

common :: [String] -> [Int] -> Setting -> String
common (x:[]) _ _ = x
common xs (_:[]) _ = head xs
common xs (pos:positions) Most =
    let res = howCommon xs pos (0,0)
    in if fst res > snd res
    then common (filter (\x -> (x !! pos) == '1') xs) positions Most
    else common (filter (\x -> (x !! pos) == '0') xs) positions Most
common xs (pos:positions) Least =
    let res = howCommon xs pos (0,0)
    in if fst res < snd res
    then common (filter (\x -> (x !! pos) == '1') xs) positions Least
    else common (filter (\x -> (x !! pos) == '0') xs) positions Least

solve :: [String] -> Int
solve input = 
    let oxygen = parseBinary $ common input [0..11] Most
        co2 = parseBinary $ common input [0..11] Least
    in oxygen * co2

main = do
    input <- getIntListInput "./input.txt"
    print $ solve $ input

