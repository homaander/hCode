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
  , findCount
  , findArr

  , findLoop
  , findLoopArr
  , findLoopId

  {-
  -- 
  , findLoops
  , findLoopsOdd
  -}

  , add
  , sub
  , neg
  ) where

import Data.List ( (\\) )
import Data.Maybe

add :: Int -> Int -> Int
add a b = (a + b) `mod` 10

neg :: Int -> Int
neg n = (10 - n) `mod` 10

sub :: Int -> Int -> Int
sub a b = add a (neg b)



type HData = [Int]

toHData :: Int -> HData
toHData n = map (\pow -> (n `div` pow) `mod` 10) powArr
  where
    len = length $ show n
    powLen = reverse [0 .. len - 1]
    powArr = map (10 ^) powLen

toHDataN :: Int -> Int -> HData
toHDataN c n = replicate (c - ldt) 0 <> dt
  where
    dt = toHData n
    ldt = length dt

fromHData :: HData -> Int
fromHData hd = sum $ zipWith (*) hd powArr
  where
    len = length hd
    powLen = reverse [0 .. len - 1]
    powArr = map (10 ^) powLen


negData :: HData -> HData
negData = map neg

sumData :: HData -> HData -> HData
sumData = zipWith add

difData :: HData -> HData -> HData
difData = zipWith sub


code :: HData -> HData
code n = map (uncurry sub) pairs
  where
    pairs = reverse $ zip n (0 : n)

decode :: HData -> HData
decode n = fst $ foldr (\e (r, a) -> (r <> [add e a], add e a)) ([], 0) n

codeN :: Int -> HData -> HData
codeN n hd = iterate code hd !! n

decodeN :: Int -> HData -> HData
decodeN n hd = iterate decode hd !! n

-- Loops
recurseCode :: HData -> HData
recurseCode hd = iterate code hd !! (hnum `mod` findLoop hd)
  where
    hnum = fromHData hd


findCount :: HData -> HData -> Maybe Int
findCount ihd hd = if res == maxlen
                   then Nothing
                   else Just res
  where
    res = foldr
      (\he n -> if he == hd then 1 else n + 1) 0
      $ take maxlen $ iterate code (code ihd)
    maxlen = 10^nu
    nu = length ihd


findArr :: HData -> HData -> Maybe [HData]
findArr ihd hd = if length res == maxlen
                 then Nothing
                 else Just res
  where
    res = ihd : foldr
      (\he n -> if he == hd then [] else [he] <> n) []
      (take maxlen $ iterate code $ code ihd)
    maxlen = 10^nu
    nu = length ihd


findLoop :: HData -> Int
findLoop hd = fromMaybe 0 (findCount hd hd)

findLoopArr :: HData -> [HData]
findLoopArr hd = fromMaybe [] (findArr hd hd)

findLoopId :: HData -> HData
findLoopId = minimum . findLoopArr


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