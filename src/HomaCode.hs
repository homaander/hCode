module HomaCode (
    code
  , decode

  , codeN
  , (^->)
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

  , findDisperce
  , findDisperceData
  , showDisperceList

  {-
  , findLoops
  , findLoopsOdd
  -}

  , add
  , sub
  , neg

  , x5z0, x5z1, x5z2, x5z3, x5z4
  , x5z5, x5z6, x5z7, x5z8, x5z9
  ) where

import Data.Maybe ( fromMaybe )
import Data.List ( nub, sort )

type HData = [Int]

-- Разложение лент
disperceMaxOffset :: Int
disperceMaxOffset = 500

-- Какие ленты можно представить из двух входящих
showDisperceList :: HData -> HData -> [Int]
showDisperceList aTape bTape = sort $ nub $ map (\n -> fromHData $ getLoopId (sumData aTape (codeN n bTape))) [0 .. disperceMaxOffset]

-- Список сещений, которые можно применить ко второй ленте, что бы в сумме с первой получить 3
findDisperce :: HData -> HData -> HData -> [Int]
findDisperce = ((map fst .) .) . findDisperceData

-- То же самое что и findDisperce но вторым аргументом картежа выдаёт общее смещение
findDisperceData :: HData -> HData -> HData -> [(Int,Int)]
findDisperceData aTape bTape resTape = filter (/= (-1,-1)) $
    map check [0 .. disperceMaxOffset]
  where
    lid = getLoopId resTape
    thr (_, _, a, _) = a
    check n = if getLoopId nsum == lid then (n, thr $ getLoop nsum) else (-1,-1)
      where
        nsum = sumData aTape (codeN n bTape)

-- Ленты (Id, Смещение, Отр. смещение, длинна)
getLoop :: HData -> (HData, Int, Int, Int)
getLoop hdata = (hid, offset, getLoopLen hdata - offset, getLoopLen hdata)
  where
    offset = fromMaybe 0 (findCodeCount hid hdata)
    hid = getLoopId hdata

getLoopArr :: HData -> [HData]
getLoopArr hdata = fromMaybe [] (findCodeArr hdata hdata)

getLoopLen :: HData -> Int
getLoopLen hdata = fromMaybe 0 (findCodeCount hdata hdata)

getLoopId :: HData -> HData
getLoopId  = minimum . getLoopArr


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

(^->) :: HData -> Int -> HData
(^->) = flip codeN


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

-- Тестовые значения
x5z0, x5z1, x5z2, x5z3, x5z4 :: HData
x5z5, x5z6, x5z7, x5z8, x5z9 :: HData
x5z0 = [0,0,0,0,0]
x5z1 = [0,0,0,0,1]
x5z2 = [0,0,0,0,2]
x5z3 = [0,0,0,0,3]
x5z4 = [0,0,0,0,4]
x5z5 = [0,0,0,0,5]
x5z6 = [0,0,0,0,6]
x5z7 = [0,0,0,0,7]
x5z8 = [0,0,0,0,8]
x5z9 = [0,0,0,0,9]

-- Отдельный тип для 5х лент
-- data Tape = T HData Int
-- t1 :: Tape
-- t1  = T [0,0,0,0,1] 0

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