module HomaCode (
    code
  , decode

  , codeN
  , decodeN

  , toHData
  , toHDataN
  , fromHData

  , negData
  , sumData
  , difData

  , recurseCode
  , findLen
  , findArr

  , findLoopLen
  , findLoopArr
  , findLoopId
  , findLoop

  {-
  , findLoops
  , findLoopsOdd
  -}

  , add
  , sub
  , neg
  ) where

import Data.Maybe

neg :: Int -> Int
neg n   = (10 - n) `mod` 10

add, sub :: Int -> Int -> Int
add a b = (a + b) `mod` 10
sub a b = add a (neg b)



type HData = [Int]

toHData :: Int -> HData
toHData num = map (\pow -> (num `div` pow) `mod` 10) powArr
  where
    len = length $ show num
    powLen = reverse [0 .. len - 1]
    powArr = map (10 ^) powLen

toHDataN :: Int -> Int -> HData
toHDataN c num = replicate (c - length dt) 0 <> dt
  where
    dt = toHData num

fromHData :: HData -> Int
fromHData hdata = sum $ zipWith (*) hdata powArr
  where
    powLen = reverse [0 .. length hdata - 1]
    powArr = map (10 ^) powLen


negData :: HData -> HData
negData = map neg

sumData, difData :: HData -> HData -> HData
sumData = zipWith add
difData = zipWith sub


code,  decode  :: HData -> HData
code hdata = map (uncurry sub) pairs
  where
    pairs = reverse $ zip hdata (0 : hdata)

decode hdata = fst $ 
  foldr (\e (r, a) -> (r <> [add e a], add e a)) ([], 0) hdata

codeN, decodeN :: Int -> HData -> HData
codeN   n hdata = iterate code hdata !! n
decodeN n hdata = iterate decode hdata !! n

-- Loops
recurseCode :: HData -> HData
recurseCode hdata = iterate code hdata !! (hnum `mod` findLoopLen hdata)
  where
    hnum = fromHData hdata


findLen :: HData -> HData -> Maybe Int
findLen ihd hdata = if res == maxlen
                       then Nothing
                       else Just res
  where
    res = foldr
      (\he n -> if he == hdata then 1 else n + 1) 0
      (take maxlen $ iterate code (code ihd))
    maxlen = 10 ^ length ihd


findArr :: HData -> HData -> Maybe [HData]
findArr ihd hdata = if length res == maxlen
                    then Nothing
                    else Just res
  where
    res = ihd : foldr
      (\he n -> if he == hdata then [] else [he] <> n) []
      (take maxlen $ iterate code $ code ihd)
    maxlen = 10 ^ length ihd


findLoopLen :: HData -> Int
findLoopLen hdata = fromMaybe 0 (findLen hdata hdata)

findLoopId :: HData -> HData
findLoopId  = minimum . findLoopArr

findLoop :: HData -> (HData, Int)
findLoop hdata = (findLoopId hdata, findLoopLen hdata)

findLoopArr :: HData -> [HData]
findLoopArr hdata = fromMaybe [] (findArr hdata hdata)


{-
findLoops :: Int -> [(HData, Int)]
findLoops n = (toHDataN n 0, 1) : res
  where
    res = findLoops' n $ map (toHDataN n) [1 .. (10^n) - 1]

findLoopsOdd :: Int -> [(HData, Int)]
findLoopsOdd n = (toHDataN n 0, 1) : res
  where
    res = findLoops' n $ map (toHDataN n) [1 .. 9]

findLoops' :: Int -> [HData] -> [(HData, Int)]
findLoops' _ [] = []
findLoops' n (x:xs) = (x, length arr) : findLoops' n (xs \\ arr)
  where
    arr = findLoopArr x
-}

-- Matrix map