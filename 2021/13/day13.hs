import Data.List
import Data.Maybe

data InputPart = Instructions |
                 Coordinates

getListInput :: String -> InputPart -> IO [String]
getListInput path Instructions = readFile path >>= return . tail . dropWhile (/="") . lines
getListInput path Coordinates = readFile path >>= return . takeWhile (/="") . lines

getCoords :: String -> (Int, Int)
getCoords s = (read $ sc !! 0, read $ sc !! 1)
    where f ',' = ' '
          f x = x
          sc = words $ map f s

data FoldStrategy = Horizontal Int |
                    Vertical Int

parseFoldStrategy :: String -> FoldStrategy
parseFoldStrategy s
    | Just a <- stripPrefix "fold along y=" s = Horizontal $ read a
    | Just a <- stripPrefix "fold along x=" s = Vertical $ read a

solve :: [FoldStrategy] -> [(Int, Int)] -> [(Int, Int)]
solve [] cs = cs
solve (f:fs) cs = solve fs $ nub $ fold cs f

fold :: [(Int, Int)] -> FoldStrategy -> [(Int, Int)]
fold [] _ = []
fold ((x, y):cs) strat@(Horizontal v) = (x, if y > v then v - (y - v) else y) : fold cs strat
fold ((x, y):cs) strat@(Vertical v) = (if x > v then v - (x - v) else x, y) : fold cs strat

display :: [String] -> [(Int, Int)] -> String
display pixels [] = unlines pixels
display pixels ((x, y):cs) = display updated cs
    where nRow = take x (pixels !! y) ++ ['#'] ++ drop (x + 1) (pixels !! y)
          updated = take y pixels ++ [nRow] ++ drop (y + 1) pixels

main = do
    instructions <- getListInput "./input.txt" Instructions >>= return . map parseFoldStrategy
    coordinates <- getListInput "./input.txt" Coordinates >>= return . map getCoords
    putStrLn $ display (take 6 $ repeat (take 40 $ repeat '.')) $ solve instructions coordinates

