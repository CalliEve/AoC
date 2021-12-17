import Debug.Trace

getListInput :: String -> IO Matrix
getListInput path = readFile path >>= return . map (map read) . map (map (\x -> [x])) . lines

type Matrix = [[Int]]

showMatrix :: Matrix -> String
showMatrix m = unlines [unwords $ map show r | r <- m]

solve :: Int -> Matrix -> Int
solve step m 
    | c == 100 = step + 1
    | otherwise = (solve (step + 1) $ trace (showMatrix u) u)
    where (c, u) = calcFlashes 0 0 0 $ map (\x -> map (\y -> y + 1) x) m

calcFlashes :: Int -> Int -> Int -> Matrix -> (Int, Matrix)
calcFlashes x y c m | length m - 1 == y && length (head m) == x = (c, m)
calcFlashes x y c m | length (head m) == x = calcFlashes 0 (y + 1) c m
calcFlashes x y c m = calcFlashes (x + 1) y cu u
    where (cu, u) = if get x y m > 9 then execFlash x y c m else (c, m)

execFlash :: Int -> Int -> Int -> Matrix -> (Int, Matrix)
execFlash x@0 y@0 c m = foldFlash x y c m [(x + 1, y), (x, y + 1), (x + 1, y + 1)]
execFlash x y@0 c m 
    | length (head m) - 1 == x = foldFlash x y c m [(x - 1, y), (x - 1, y + 1), (x, y + 1)]
    | otherwise = foldFlash x y c m [(x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
execFlash x@0 y c m
    | length m - 1 == y = foldFlash x y c m [(x + 1, y), (x, y - 1), (x + 1, y - 1)]
    | otherwise = foldFlash x y c m [(x + 1, y), (x, y - 1), (x + 1, y - 1), (x, y + 1), (x + 1, y + 1)]
execFlash x y c m
    | length m - 1 == y && length (m !! 1) - 1 == x = foldFlash x y c m [(x - 1, y), (x - 1, y - 1), (x, y - 1)]
    | length m - 1 == y = foldFlash x y c m [(x - 1, y), (x + 1, y), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
    | length (head m) - 1 == x =  foldFlash x y c m [(x - 1, y), (x - 1, y - 1), (x, y - 1), (x - 1, y + 1), (x, y + 1)]
    | otherwise = foldFlash x y c m [(x - 1, y), (x + 1, y), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

foldFlash :: Int -> Int -> Int -> Matrix -> [(Int, Int)] -> (Int, Matrix)
foldFlash x y c m poses = foldl (\(c, m) (x, y) -> up1Octopus x y c m) (c + 1, set x y 0 m) poses

up1Octopus :: Int -> Int -> Int -> [[Int]] -> (Int, [[Int]])
up1Octopus x y c m | get x y m == 0 = (c, m)
up1Octopus x y c m | get x y u > 9 = execFlash x y c u
                   | otherwise = (c, u)
                   where v = get x y m
                         u = set x y (v + 1) m

set :: Int -> Int -> Int -> Matrix -> Matrix
set x y val m = nm
    where (sy, ey) = splitAt y m
          (s, e) = splitAt x $ head ey
          nxs = (s ++ [val]) ++ tail e
          nm = (sy ++ [nxs]) ++ tail ey

get :: Int -> Int -> Matrix -> Int
get x y m = m !! y !! x 

main = getListInput "./input.txt" >>= print . solve 0

