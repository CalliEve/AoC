import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative
import Data.List

data Pair =
    Num Int
    | Nest [Pair]

instance Show Pair where
    show (Num x) = show x
    show (Nest [l, r]) = show [l, r]

parse :: ReadP Pair
parse = do
    p <- parseNum <|> parseNest
    return p

parseNum :: ReadP Pair
parseNum = do
    p <- munch1 isDigit
    return $ Num $ read p

parseNest :: ReadP Pair
parseNest = do
    char '['
    l <- parse
    char ','
    r <- parse 
    char ']'
    return $ Nest [l, r]

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

thd4 :: (a, b, c, d) -> c 
thd4 (_, _, c, _) = c

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

readInputLines :: String -> IO [Pair]
readInputLines path = readFile path >>= return . map (fst . head . readP_to_S parse) . lines

explodePair :: Int -> Pair -> (Pair, Maybe Pair, Maybe Pair, Bool)
explodePair i (Nest [Num l, Num r]) | i >= 4 = (Num 0, Just (Num l), Just (Num r), True)
explodePair i (Nest [l, r]) = (Nest [nl, nr], lm, thd4 rres, f || fth4 rres)
    where (lres, lm, rm, f) = explodePair (i + 1) l
          rres = maybe (if f then (r, Nothing, Nothing, True) else explodePair (i + 1) r) (\x -> (addNums x r, Nothing, Nothing, True)) rm
          nl = maybe lres (\x -> addNums lres x) (snd4 rres)
          nr = fst4 rres
explodePair _ p = (p, Nothing, Nothing, False)

splitPair :: Pair -> (Pair, Bool)
splitPair (Num n) | n > 9 = (Nest [Num $ n `div` 2, Num $ if n `mod` 2 /= 0 then n `div` 2 + 1 else n `div` 2], True)
splitPair n@(Num _) = (n, False)
splitPair (Nest [l, r]) = (Nest [lres, rres], rf)
    where (lres, lf) = splitPair l
          (rres, rf) = if lf then (r, True) else splitPair r

addNums :: Pair -> Pair -> Pair
addNums (Num l) (Num r) = Num (l + r)
addNums n@(Num _) (Nest [l, r]) = Nest [addNums n l, r]
addNums (Nest [l, r]) n@(Num _) = Nest [l, addNums r n]

reducePair :: Pair -> Pair
reducePair p = if snd sres then reducePair $ fst sres else fst sres
    where eres = explodePair 0 p
          sres = if not $ fth4 eres then splitPair $ fst4 eres else (fst4 eres, fth4 eres)

calcMagnitude :: Pair -> Int
calcMagnitude (Num n) = n
calcMagnitude (Nest [l, r]) = (3 * calcMagnitude l) + (2 * calcMagnitude r)

getPermutations :: [Pair] -> [Int]
getPermutations [_] = []
getPermutations (p:ps) = map (\x -> calcMagnitude $ reducePair $ Nest [x, p]) ps ++ map (\x -> calcMagnitude $ reducePair $ Nest [p, x]) ps ++ getPermutations ps

main = readInputLines "./input.txt" >>= print . maximum . getPermutations

