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

  , recCodeOffset
  , recCode

  , findLen
  , findArr

  , nextNCode

  , findLoopLen
  , findLoopArr
  , findLoopId
  , findLoop

  , haveIn500l
  , haveIn500r
  , showIn500l

  {-
  , findLoops
  , findLoopsOdd
  -}

  , add
  , sub
  , neg
  ) where

import Data.Maybe ( fromMaybe )

type HData = [Int]

showIn500l :: HData -> HData -> [Int]
showIn500l a b = map (\n -> fromHData $ findLoopId (sumData a (codeN n b))) [0 .. 500]

haveIn500r :: HData -> HData -> HData -> [Int]
haveIn500r a b c = filter (>= 0) $
    map (\n -> if findLoopId (sumData a (codeN n b)) == lid then n else -1) [0 .. 500]
  where
    lid = findLoopId c

haveIn500l :: HData -> HData -> HData -> [Int]
haveIn500l a b c = filter (>= 0) $
    map (\n -> if findLoopId (sumData (codeN n a) b) == lid then n else -1) [0 .. 500]
  where
    lid = findLoopId c

-- Ленты
findLen :: HData -> HData -> Maybe Int
findLen ihd hdata = if   res == maxlen
                    then Nothing
                    else Just res
  where
    res = foldr
      (\he n -> if he == hdata then 1 else n + 1) 0
      (nextNCode maxlen ihd)
    maxlen = 10 ^ length ihd


findArr :: HData -> HData -> Maybe [HData]
findArr ihd hdata = if   length res == maxlen
                    then Nothing
                    else Just res
  where
    res = ihd : foldr
      (\he n -> if he == hdata then [he] else [he] <> n) []
      (nextNCode maxlen ihd)
    maxlen = 10 ^ length ihd


findLoopLen :: HData -> Int
findLoopLen hdata = fromMaybe 0 (findLen hdata hdata)

findLoopId :: HData -> HData
findLoopId  = minimum . findLoopArr

findLoop :: HData -> (HData, Int)
findLoop hdata = (findLoopId hdata, findLoopLen hdata)

findLoopArr :: HData -> [HData]
findLoopArr hdata = fromMaybe [] (findArr hdata hdata)


-- Рекурсивные кодировки
recCodeOffset :: HData -> Int
recCodeOffset hdata = fromHData hdata `mod` findLoopLen hdata

recCode :: HData -> HData
recCode hdata = codeN (recCodeOffset hdata) hdata

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