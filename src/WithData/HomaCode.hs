module WithData.HomaCode (
    Math(..)
  , Code(..)
  , CodeRecurse(..)
  , Tapes(..)
  , DataSet(..)

  , showDisperseList
  , findDisperse
  , findDisperseData
) where

import WithData.HomaCodeData
import WithData.HomaCodeClasses

import Data.Maybe ( fromMaybe )
import Data.List ( nub, sort )

-- ! to class
showDisperseList :: [Int] -> [Int] -> [Int]
showDisperseList aTape bTape = sort $ nub $ map (\n -> fromHData $ getTapeId (add aTape (codeN n bTape))) [0 .. 500]

findDisperse :: [Int] -> [Int] -> [Int] -> [Int]
findDisperse = ((map fst .) .) . findDisperseData

-- То же самое что и findDisperce но вторым аргументом картежа выдаёт общее смещение
findDisperseData :: [Int] -> [Int] -> [Int] -> [(Int,Int)]
findDisperseData aTape bTape resTape = filter (/= (-1,-1)) $
    map check [0 .. 500]
  where
    lid = getTapeId resTape
    thr (_, _, a, _) = a
    check n = if getTapeId nsum == lid then (n, tapeAntiOffset $ toTape nsum) else (-1,-1)
      where
        nsum = aTape ^+ codeN n bTape


-- DataSet instance
instance DataSet [] where
  fromHData hdata = sum $ zipWith (*) (map fromEnum hdata) powArr
    where
      powArr = map (10 ^) powLen
      powLen = reverse [0 .. length hdata - 1]
  toHData num = map (toEnum . (`mod` 10) . (num `div`)) powArr
    where
      powArr = map (10 ^) powLen
      powLen = reverse [0 .. len - 1]
      len    = length $ show num
  toHDataN count num = replicate (count - length dt) zero <> dt
    where
      dt = toHData num

instance CodeRecurse [] where
  codeRecurse hdata = codeN (fromHData hdata) hdata
  nextNCode n ihd = take n $ iterate code (code ihd)

-- >>>  WithData.HomaCodeFunction.toHDataN 6 123 :: [Int]
-- [0,0,0,1,2,3]

-- Math instance
instance Math Int where
  add a b = (a + b) `mod` 10
  neg n   = (10 - n) `mod` 10
  zero = 0

instance Math HNums16 where
  add a b = toEnum $ (fromEnum a + fromEnum b) `mod` 16
  neg n   = toEnum $ (16 - fromEnum n) `mod` 16
  zero = H00

instance Math a => Math [a] where
  add = zipWith add
  neg = map neg
  zero = []


-- Code 
instance Math a => Code [a] where
  code hdata = map (uncurry sub) pairs
    where
      pairs = reverse $ zip hdata (zero : hdata)

  decode hdata = fst $ 
    foldr (\e (r, a) -> (r <> [e ^+ a], e ^+ a)) ([], zero) hdata

-- >>> [H12,H02,H13,H04,H06]
-- >>> WithData.HomaCodeFunction.code [H12,H02,H13,H04,H06]
-- [C,2,D,4,6]
-- [2,7,B,6,C]

-- >>> [H12,H02,H13,H03,H14,H04]
-- >>> WithData.HomaCodeFunction.codeN 66 [H12,H02,H13,H03,H14,H04]
-- [C,2,D,3,E,4]
-- [5,8,4,C,3,2]

-- >>> WithData.HomaCodeFunction.decode ([1,2,5] :: [Int])
-- [5,7,8]

-- >>> WithData.HomaCodeFunction.decodeN 2 ([1,2,3] :: [Int])
-- [6,1,4]

instance (Ord a, Enum a, Math a) => Tapes [a] where
  toTape hdata = HTape hid offset (getTapeLength hdata - offset) (getTapeLength hdata)
    where
      offset = fromMaybe 0 (findCodeOffset hid hdata)
      hid = getTapeId hdata

  findCodeOffset ihd hdata = if res == maxlen
                          then Nothing
                          else Just res
    where
      res = foldr
        (\he n -> if he == hdata then 1 else n + 1) 0
        (nextNCode maxlen ihd)
      maxlen = 10 ^ length ihd

  findCodeList ihd hdata = if length res == maxlen
                        then Nothing
                        else Just res
    where
      res = ihd : foldr
        (\he n -> if he == hdata then [he] else [he] <> n) []
        (nextNCode maxlen ihd)
      maxlen = 10 ^ length ihd

-- >>> WithData.HomaCodeFunction.toTape ([0,1,2,3,4] :: [Int])
-- HTape {tapeId = [0,0,0,0,1], tapeOffset = 17811, tapeAntiOffset = 6400, tapeLength = 24211}