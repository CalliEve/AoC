import Data.List
import Data.Char

data Player = Player {
    pNum :: Int,
    pos :: Int,
    score :: Int,
    turn :: Bool,
    won :: Bool}
    deriving (Eq, Show)

getInput :: String -> IO [Player]
getInput path = readFile path >>= return . map (\(i, p) -> Player i p 0 False False) . map (\l -> (digitToInt $ head l,read $ dropWhile (not . isDigit) $ tail l)) . map (dropWhile (not . isDigit)) . lines

playTurn :: (Player, Player) -> [(Int, Int)]
playTurn (p1, p2) 
    | turn p1 = [y | x<-map (\(t, p) -> map (mulThrow t) $ if won p then [(1, 0)] else playTurn (p, giveNextTurn p2)) $ map (\t -> let p = position (pos p1) t; s = score p1 + p in (t, Player (pNum p1) p s False (s >= 21))) $ throws, y<-x]
    | turn p2 = [y | x<-map (\(t, p) -> map (mulThrow t) $ if won p then [(0, 1)] else playTurn (giveNextTurn p1,p)) $ map (\t -> let p = position (pos p2) t; s = score p2 + p in (t, Player (pNum p2) p s False (s >= 21))) $ throws, y<-x]
    where position pos rolls = (pos + rolls) `nmod` 10

throws :: [Int]
throws = nub $ map sum $ t 3
    where t 1 = [[1], [2], [3]]
          t n = [x:y | x<-[1, 2, 3], y<-(t (n - 1))]

giveNextTurn :: Player -> Player
giveNextTurn (Player n pos score _ w) = Player n pos score True w

nmod :: Int -> Int -> Int
nmod a b = if m == 0 then b else m
    where m = a `mod` b

mulThrow :: Int -> (Int, Int) -> (Int, Int)
mulThrow t (p1s, p2s) | t == 3 || t == 9 = (p1s, p2s)
mulThrow t (p1s, p2s) | t == 4 || t == 8 = (p1s * 3, p2s * 3)
mulThrow t (p1s, p2s) | t == 5 || t == 7 = (p1s * 6, p2s * 6)
mulThrow 6 (p1s, p2s) = (p1s * 7, p2s * 7)

main = do
    [(Player n pos score _ _), p2] <- getInput "./input.txt"
    let (p1s, p2s) = foldl (\(p1s, p2s) (p1, p2)-> (p1s + p1, p2s + p2)) (0,0) $ playTurn (Player n pos score True False, p2)
    print $ max p1s p2s

