module WithData.HomaCodeFunction (
    Math(..)
  , Code(..)
  , CodeRecurse(..)
  , Tapes(..)
  , DataSet(..)
) where

import WithData.HomaCodeData ( HTape(..), HNums16(..) )

class DataSet f where
  fromHData :: Enum a => f a -> Int
  toHData   :: Enum a => Int -> f a
  toHDataN  :: (Math a, Enum a) => Int -> Int -> f a


class Show a => Math a where
  -- instance
  add :: a -> a -> a
  neg :: a -> a

  zero :: a

  -- default
  sub :: a -> a -> a
  sub a b = add a (neg b)

  (^+) :: a -> a -> a
  (^+) = add

  (^-) :: a -> a -> a
  (^-) = sub


class Math a => Code a where
  -- instance
  code :: a -> a
  decode :: a -> a

  -- default
  codeN :: Int -> a -> a
  codeN n hdata = iterate code hdata !! n
  decodeN :: Int -> a -> a
  decodeN n hdata = iterate decode hdata !! n

  (^->) :: a -> Int -> a
  (^->) = flip codeN

class DataSet f => CodeRecurse f where
  codeRecurse :: (Enum a, Code a) => f a -> f a

class Code a => Tapes a where
  -- instance
  toTape :: a -> HTape a

  -- default
  fromTape :: HTape a -> a
  fromTape (HTape h n _) = codeN n h


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

-- >>>  WithData.HomaCodeFunction.toHDataN 6 123 :: [Int]
-- [0,0,0,1,2,3]

-- >>>  WithData.HomaCodeFunction.toHDataN 6 123 :: [HNums16]
-- Not in scope: type constructor or class `HData16'

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

instance CodeRecurse [] where
  codeRecurse hdata = codeN (fromHData hdata) hdata

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
