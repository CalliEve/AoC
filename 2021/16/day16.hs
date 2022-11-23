import Debug.Trace

data Packet = 
    Packet {
        packetVersion :: Int,
        packetID :: Int,
        packetLengthType :: Int,
        subPackets :: [Packet],
        packetValue :: Int,
        packetLength :: Int,
        packetVersionSum :: Int} 
    deriving (Show)

parseBinary :: String -> Int
parseBinary [] = 0
parseBinary (x:xs) = (if x == '1' then 2^(length xs) else 0) + parseBinary xs

toBinary :: String -> String
toBinary [] = []
toBinary ('0':s) = "0000" ++ toBinary s
toBinary ('1':s) = "0001" ++ toBinary s
toBinary ('2':s) = "0010" ++ toBinary s
toBinary ('3':s) = "0011" ++ toBinary s
toBinary ('4':s) = "0100" ++ toBinary s
toBinary ('5':s) = "0101" ++ toBinary s
toBinary ('6':s) = "0110" ++ toBinary s
toBinary ('7':s) = "0111" ++ toBinary s
toBinary ('8':s) = "1000" ++ toBinary s
toBinary ('9':s) = "1001" ++ toBinary s
toBinary ('A':s) = "1010" ++ toBinary s
toBinary ('B':s) = "1011" ++ toBinary s
toBinary ('C':s) = "1100" ++ toBinary s
toBinary ('D':s) = "1101" ++ toBinary s
toBinary ('E':s) = "1110" ++ toBinary s
toBinary ('F':s) = "1111" ++ toBinary s
toBinary (_:s) = toBinary s

parseLiterals :: String -> [String]
parseLiterals ('0':s) = [take 4 s]
parseLiterals ('1':s) = take 4 s : (parseLiterals $ drop 4 s)

combineLiterals :: [String] -> Int
combineLiterals ss = parseBinary $ concat ss

parseSubPackets :: Int -> String -> [Int]
parseSubPackets i str | i < 22 && length str >= i = [parseBinary $ take i str]
                    | otherwise = (parseBinary $ take 11 str) : parseSubPackets (i - 11) (drop 11 str)

decreaseIfNum :: Maybe Int -> Maybe Int
decreaseIfNum (Just i) = Just (i - 1)
decreaseIfNum Nothing = Nothing

calcValue :: Int -> [Packet] -> Int
calcValue 0 ps = sum $ map packetValue ps
calcValue 1 ps = product $ map packetValue ps
calcValue 2 ps = minimum $ map packetValue ps
calcValue 3 ps = maximum $ map packetValue ps
calcValue 5 ps = if (packetValue $ head ps) > (packetValue $ last ps) then 1 else 0
calcValue 6 ps = if (packetValue $ head ps) < (packetValue $ last ps) then 1 else 0
calcValue 7 ps = if (packetValue $ head ps) == (packetValue $ last ps) then 1 else 0

parse :: Maybe Int -> String -> [Packet]
parse (Just 0) _ = []
parse _ [] = []
parse _ str | all (=='0') str = []
parse i str
    | ('1':'0':'0':_) <- drop 3 str = packet : (parse (decreaseIfNum i) $ drop l str)
    where version = parseBinary $ take 3 str
          literals = parseLiterals $ drop 6 str
          value = combineLiterals literals
          l = 6 + length literals * 5
          packet = Packet {packetVersion=version, packetID=4, packetValue=value, packetLength=l, packetVersionSum=version, subPackets=[]}
parse i str
    | ('0':_) <- drop 6 str = packet : (parse (decreaseIfNum i) $ drop l str)
    where version = parseBinary $ take 3 str
          id = parseBinary $ take 3 $ drop 3 str
          lengthSub = parseBinary $ take 15 $ drop 7 str
          subs = parse Nothing $ take lengthSub $ drop 22 str
          l = 22 + lengthSub
          value = calcValue id subs
          packet = Packet {packetVersion=version, packetID=id, packetLengthType=0, subPackets=subs, packetLength=l, packetVersionSum=version + (sum $ map (\x -> packetVersionSum x) subs), packetValue=value}
parse i str
    | ('1':_) <- drop 6 str = packet : (parse (decreaseIfNum i) $ drop l str)
    where version = parseBinary $ take 3 str
          id = parseBinary $ take 3 $ drop 3 str
          subCount = parseBinary $ take 11 $ drop 7 str
          subs = parse (Just subCount) $ drop 18 str
          l = 18 + (sum $ map (\x -> packetLength x) subs)
          value = calcValue id subs
          packet = Packet {packetVersion=version, packetID=id, packetLengthType=1, subPackets=subs, packetLength=l, packetVersionSum=version + (sum $ map (\x -> packetVersionSum x) subs), packetValue=value}

main = readFile "./input.txt" >>= print . packetValue . head . parse Nothing . toBinary

