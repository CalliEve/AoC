import Data.List
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP
import Control.Applicative
import Debug.Trace

data Switch = 
    On Cube
    | Off Cube
    deriving (Show, Eq)

getCube :: Switch -> Cube
getCube (On cube) = cube
getCube (Off cube) = cube

data Cube = Cube {
    x :: (Integer, Integer),
    y :: (Integer, Integer),
    z :: (Integer, Integer)
}
    deriving (Show, Eq)

parseSwitch :: ReadP Switch
parseSwitch = do
    m <- string "on" <|> string "off"
    string " x="
    x <- parseCoord
    string ",y="
    y <- parseCoord
    string ",z="
    z <- parseCoord
    return $ if m == "on" then On $ Cube x y z else Off $ Cube x y z

parseCoord :: ReadP (Integer, Integer)
parseCoord = do
    a <- munch1 (\x -> isDigit x || x == '-')
    string ".."
    b <- munch1 (\x -> isDigit x || x == '-')
    return (read a, read b)

toggleSwitch :: Switch -> Switch
toggleSwitch (On c) = Off c
toggleSwitch (Off c) = On c

getInput :: String -> IO [Switch]
getInput path = readFile path >>= return . map (fst . head . readP_to_S parseSwitch) . lines

getSize :: Cube -> Integer
getSize (Cube (x1,x2) (y1,y2) (z1,z2)) = ((x2 + 1)-x1) * ((y2 + 1)-y1) * ((z2 + 1)-z1)

getOverlap :: Cube -> Cube -> Maybe Cube
getOverlap (Cube x1 y1 z1) (Cube x2 y2 z2) = if any (==0) [fst x, fst y, fst z] then Nothing else Just $ Cube x y z
    where x = getOverlapLine x1 x2
          y = getOverlapLine y1 y2
          z = getOverlapLine z1 z2

getOverlapLine :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
getOverlapLine (a1, a2) (b1, b2)
    | a2 <= b2 && a1 >= b1 = (a1, a2)
    | a2 >= b2 && a1 <= b1 = (b1, b2)
    | a2 <= b2 && a2 >= b1 = (b1, a2)
    | a2 >= b2 && a1 <= b2 = (a1, b2)
    | otherwise = (0,0)

getOverlapSize :: Switch -> Switch -> [Switch] -> Integer
getOverlapSize s1 s2 ss = maybe 0 (\x -> calcAddedSize (ms s2 x) ss) o
    where o = getOverlap (getCube s1) (getCube s2)
          ms (On _) n = On n
          ms (Off _) n = Off n

calcAddedSize :: Switch -> [Switch] -> Integer
calcAddedSize (Off _) [] = 0
calcAddedSize s [] = getSize $ getCube s
calcAddedSize s (s2:ss) = (calcAddedSize s ss) - getOverlapSize s s2 ss

solve :: [Switch] -> [Switch] -> Integer
solve _ [] = 0
solve st (s:ss) = trace ("acc: " ++ (show sum) ++ ", added: " ++ (show n)) $ n + sum
    where sum = solve (s:st) ss
          n = calcAddedSize s st

main = getInput "./input.txt" >>= print . solve []

