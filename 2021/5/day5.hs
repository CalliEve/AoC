import Data.List

getListInput :: String -> IO [(Point, Point)]
getListInput path = do
    content <- readFile path
    return $ parse $ lines content

parse :: [String] -> [(Point,Point)]
parse = map (parseLine . words)

parseLine :: [String] -> (Point,Point)
parseLine [ s, "->", e ] = (a, b)
    where a = parseCoord s
          b = parseCoord e

toRange :: (Point,Point) -> [Point]
toRange (a @ (xa,ya), b @ (xb,yb))
    | diagonal (a,b) = makeDiagonal a b
    | otherwise      = [ (x,y) | x <- [ x1 .. x2 ], y <- [ y1 .. y2 ] ]
    where
        x1 = min xa xb
        x2 = max xa xb
        y1 = min ya yb
        y2 = max ya yb

parseCoord :: String -> (Int,Int)
parseCoord l = (head n, last n)
    where
        n = map read . words $ map f $ l
        f ',' = ' '
        f x = x

solve :: [(Point, Point)] -> [Point]
solve [_] = []
solve (p:ps) = overlapping' p ps ++ solve ps

overlapping' :: (Point, Point) -> [(Point, Point)] -> [Point]
overlapping' _ [] = []
overlapping' p1 (p2:ps) = overlap p1 p2 ++ overlapping' p1 ps

diagonal :: (Point,Point) -> Bool
diagonal (a,b) = xof a /= xof b && yof a /= yof b

type Point = (Int,Int)

xof :: Point -> Int
xof = fst

yof :: Point -> Int
yof = snd

overlap :: (Point,Point) -> (Point,Point) -> [Point]
overlap a @ (sa,ea) b @ (sb,eb) 
    | x && y    = toRange a `intersect` toRange b
    | otherwise = []
    where
        x = xof sa >= xof sb || xof sa <= xof sb
        y = yof sa >= yof sb || yof sa <= yof sb

makeDiagonal :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
makeDiagonal (a, b) (c, d)
    | (a > c && b > d) || (a < c && b < d) = 
        let x1 = min a c
            x2 = max a c
            y1 = min b d
        in [(x1 + i, y1 + i) | i <- [0..(x2 - x1)]]
    | otherwise = 
        let x1 = min a c
            x2 = max a c
            y1 = max b d
        in [(x1 + i, y1 - i) | i <- [0..(x2 - x1)]]

main = do
    input <- getListInput "./input.txt"
    print $ length $ nub $ solve input 
