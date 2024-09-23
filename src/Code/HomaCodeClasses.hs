{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Code.HomaCodeClasses (
    Math(..)
  , Code(..)
  , CodeRecurse(..)
  , Tapes(..)
  , HData(..)
  , HDataInfo(..)
) where

import Code.HomaCodeData
import Data.Maybe ( fromMaybe )
import Data.List  ( nub )



class Show a => Math a where
  add  :: a -> a -> a
  neg  :: a -> a
  zero :: a

  getMod :: Int

  -- default
  sub :: a -> a -> a
  sub a b = add a (neg b)

  (^+) :: a -> a -> a
  (^+) = add

  (^-) :: a -> a -> a
  (^-) = sub


class Math a => HData a where
  fromHData   :: a -> Int
  toHData     :: Int -> a
  toHDataN    :: Int -> Int -> a


class Math a => Code a where
  code :: a -> a
  decode :: a -> a

  -- default
  codeN :: Int -> a -> a
  codeN n hdata = iterate code hdata !! n

  nextNCode :: Int -> a -> [a]
  nextNCode n ihd = take n $ iterate code (code ihd)

  decodeN :: Int -> a -> a
  decodeN n hdata = iterate decode hdata !! n

  (^->) :: a -> Int -> a
  (^->) = flip codeN


class (Code a, HData a) => CodeRecurse a where
  -- default
  codeRecurse :: a -> a
  codeRecurse hdata = codeN (fromHData hdata) hdata


class (Ord a, Code a) => Tapes a where
  toTape :: a -> HTape a

  findCodeOffset :: a -> a -> Maybe Int
  findCodeList   :: a -> a -> Maybe [a]

  -- default
  fromTape :: HTape a -> a
  fromTape (HTape h n _ _) = codeN n h

  getTapeId   :: a -> a
  getTapeId = minimum . getTapeList

  getTapeLength :: a -> Int
  getTapeLength hdata = fromMaybe 0 (findCodeOffset hdata hdata)

  getTapeList :: a -> [a]
  getTapeList hdata = fromMaybe [] (findCodeList hdata hdata)


class (CodeRecurse a, Tapes a) => HDataInfo a where
  -- Get tape_id list from sums with offset second 0-500
  showDisperseList :: a -> a -> [a]
  -- Get (offset b, anti-offset c) -> (a_0 + b_offset)_anti-offset = c_0
  findDisperseData :: a -> a -> a -> [(Int,Int)]

-- Math
instance Math Int where
  add a b = (a + b) `mod` 10
  neg n   = (10 - n) `mod` 10
  zero = 0
  getMod = 9

instance Math HNums16 where
  add a b = toEnum $ (fromEnum a + fromEnum b) `mod` 16
  neg n   = toEnum $ (16 - fromEnum n) `mod` 16
  zero = H00
  getMod = 16

instance Math a => Math [a] where
  add a b = zipWith add (zerosA <> a) (zerosB <> b)
    where 
      resLen = max (length a) (length b)
      zerosA = replicate (resLen - length a) zero
      zerosB = replicate (resLen - length b) zero
  neg = map neg
  zero = [zero]
  getMod = 1


-- HData
instance (Math a, Enum a) => HData [a] where
  fromHData [] = 0
  fromHData hdata = sum $ zipWith (*) (map fromEnum hdata) powArr
    where
      powArr = map (getMod @a ^) powLen
      powLen = reverse [0 .. length hdata - 1]
  toHData num = map (toEnum . (`mod` getMod @a) . div num) powArr
    where
      powArr = map (getMod @a ^) powLen
      powLen = reverse [0 .. len - 1]
      len    = length $ show num
  toHDataN count num = replicate (count - length dt) zero <> dt
    where
      dt = toHData num


-- Code 
instance Math a => Code [a] where
  code hdata = map (uncurry sub) pairs
    where
      pairs = reverse $ zip hdata (zero : hdata)

  decode hdata = fst $ 
    foldr (\e (r, a) -> (r <> [e ^+ a], e ^+ a)) ([], zero) hdata


-- RecurseCode
instance (Enum a, Math a) => CodeRecurse [a]


-- Tapes
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


-- HDataInfo
instance (Eq a, Ord a, Enum a, Math a) => HDataInfo [a] where
  showDisperseList aTape bTape = nub $ map (\n -> getTapeId (add aTape $ codeN n bTape)) [0 .. 500]

  findDisperseData aTape bTape resTape = filter (/= (-1,-1)) $
      map check [0 .. 500]
    where
      lid = getTapeId resTape
      check n = if getTapeId nsum == lid then (n, tapeAntiOffset $ toTape nsum) else (-1,-1)
        where
          nsum = aTape ^+ codeN n bTape

