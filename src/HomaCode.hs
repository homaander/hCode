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

  , recCode

  , nextNCode
  , findCodeCount
  , findCodeArr

  , getLoopLen
  , getLoopArr
  , getLoopId
  , getLoop

  , haveIn500l
  , haveIn500r
  , showIn500r

  {-
  , findLoops
  , findLoopsOdd
  -}

  , add
  , sub
  , neg

  , t0,t1,t2,t3,t4,t5,t6,t7,t8,t9
  ) where

import Data.Maybe ( fromMaybe )
import Data.List ( nub, sort )

type HData = [Int]

t0,t1,t2,t3,t4,t5,t6,t7,t8,t9 :: HData
t0 = [0,0,0,0,0]
t1 = [0,0,0,0,1]
t2 = [0,0,0,0,2]
t3 = [0,0,0,0,3]
t4 = [0,0,0,0,4]
t5 = [0,0,0,0,5]
t6 = [0,0,0,0,6]
t7 = [0,0,0,0,7]
t8 = [0,0,0,0,8]
t9 = [0,0,0,0,9]

-- Суммы лент
showIn500r :: HData -> HData -> [Int]
showIn500r aTape bTape = sort $ nub $ map (\n -> fromHData $ getLoopId (sumData aTape (codeN n bTape))) [0 .. 500]

haveIn500r :: HData -> HData -> HData -> [Int]
haveIn500r aTape bTape resTape = filter (>= 0) $
    map (\n -> if getLoopId (sumData aTape (codeN n bTape)) == lid then n else -1) [0 .. 500]
  where
    lid = getLoopId resTape

haveIn500l :: HData -> HData -> HData -> [Int]
haveIn500l aTape bTape resTape = filter (>= 0) $
    map (\n -> if getLoopId (sumData (codeN n aTape) bTape) == lid then n else -1) [0 .. 500]
  where
    lid = getLoopId resTape

-- Ленты
getLoopLen :: HData -> Int
getLoopLen hdata = fromMaybe 0 (findCodeCount hdata hdata)

getLoopId :: HData -> HData
getLoopId  = minimum . getLoopArr

getLoop :: HData -> (HData, Int, Int, Int)
getLoop hdata = (hid, offset, getLoopLen hdata - offset, getLoopLen hdata)
  where
    offset = fromMaybe 0 (findCodeCount hid hdata)
    hid = getLoopId hdata

getLoopArr :: HData -> [HData]
getLoopArr hdata = fromMaybe [] (findCodeArr hdata hdata)


-- Поиск преобразований
findCodeCount :: HData -> HData -> Maybe Int
findCodeCount ihd hdata = if res == maxlen
                          then Nothing
                          else Just res
  where
    res = foldr
      (\he n -> if he == hdata then 1 else n + 1) 0
      (nextNCode maxlen ihd)
    maxlen = 10 ^ length ihd


findCodeArr :: HData -> HData -> Maybe [HData]
findCodeArr ihd hdata = if length res == maxlen
                        then Nothing
                        else Just res
  where
    res = ihd : foldr
      (\he n -> if he == hdata then [he] else [he] <> n) []
      (nextNCode maxlen ihd)
    maxlen = 10 ^ length ihd

-- Рекурсивные кодировки

recCode :: HData -> HData
recCode hdata = codeN (fromHData hdata) hdata

nextNCode :: Int -> HData -> [HData]
nextNCode n ihd = take n $ iterate code (code ihd)

-- Кодирование / декодирование
code,  decode  :: HData -> HData
code hdata = map (uncurry sub) pairs
  where
    pairs = reverse $ zip hdata (0 : hdata)

decode hdata = fst $ 
  foldr (\e (r, a) -> (r <> [add e a], add e a)) ([], 0) hdata

codeN, decodeN :: Int -> HData -> HData
codeN   n hdata = iterate code   hdata !! n
decodeN n hdata = iterate decode hdata !! n


-- Мат. операции с данными
sumData, difData :: HData -> HData -> HData
sumData = zipWith add
difData = zipWith sub

negData :: HData -> HData
negData = map neg


-- Приведение данных
toHData :: Int -> HData
toHData num = map ((`mod` 10) . (num `div`)) powArr
  where
    powArr = map (10 ^) powLen
    powLen = reverse [0 .. len - 1]
    len = length $ show num

toHDataN :: Int -> Int -> HData
toHDataN c num = replicate (c - length dt) 0 <> dt
  where
    dt = toHData num

fromHData :: HData -> Int
fromHData hdata = sum $ zipWith (*) hdata powArr
  where
    powArr = map (10 ^) powLen
    powLen = reverse [0 .. length hdata - 1]


-- Мат. операции с числами
add, sub :: Int -> Int -> Int
add a b = (a + b) `mod` 10
sub a b = add a (neg b)

neg :: Int -> Int
neg n   = (10 - n) `mod` 10

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