module WithData.HomaCodeFunction (
    Math(..)
  , Code(..)
  , Tapes(..)
  , DataSet(..)
) where

import WithData.HomaCodeData ( HTape(..), HDataX(..), HNums16(..) )

class DataSet h where
  fromHData :: h Int -> Int
  toHData   :: Int -> h Int
  toHDataN  :: Int -> Int -> h Int

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


class Code a => Tapes a where
  -- instance
  toTape :: a -> HTape a

  -- default
  fromTape :: HTape a -> a
  fromTape (HTape h n _) = codeN n h


-- DataSet instance
instance DataSet HDataX where
  fromHData (HDX hdata) = sum $ zipWith (*) hdata powArr
    where
      powArr = map (10 ^) powLen
      powLen = reverse [0 .. length hdata - 1]
  toHData num = HDX $ map ((`mod` 10) . (num `div`)) powArr
    where
      powArr = map (10 ^) powLen
      powLen = reverse [0 .. len - 1]
      len = length $ show num
  toHDataN c num = HDX $ replicate (c - length dt) 0 <> dt
    where
      (HDX dt) = toHData num

-- >>>  WithData.HomaCodeFunction.toHDataN 6 123 :: HDataX Int
-- [0,0,0,1,2,3]

-- Math instance
instance Math Int where
  add a b = (a + b) `mod` 10
  neg n   = (10 - n) `mod` 10
  zero = 0

instance Math HNums16 where
  add a b = toEnum $ (fromEnum a + fromEnum b) `mod` 16
  neg n   = toEnum $ (16 - fromEnum n) `mod` 16
  zero = H0

instance Math a => Math (HDataX a) where
  add (HDX a) (HDX b) = HDX $ zipWith add a b
  neg (HDX a) = HDX $ map neg a
  zero = HDX [zero]


-- Code 
instance Math a => Code (HDataX a) where
  code (HDX hdata) = HDX $ map (uncurry sub) pairs
    where
      pairs = reverse $ zip hdata (zero : hdata)

  decode (HDX hdata) = HDX $ fst $ 
    foldr (\e (r, a) -> (r <> [add e a], add e a)) ([], zero) hdata

-- >>> HDX [H12,H2,H13]
-- >>> WithData.HomaCodeFunction.code (HDX [H12,H2,H13])
-- [C,2,D]
-- [B,6,C]

-- >>> HDX [H12,H2,H13,H3,H14,H4]
-- >>> WithData.HomaCodeFunction.codeN 66 (HDX [H12,H2,H13,H3,H14,H4])
-- [C,2,D,3,E,4]
-- [5,8,4,C,3,2]

-- >>> WithData.HomaCodeFunction.decode (HDX [1,2,3] :: HDataX Int)
-- [3,5,6]

-- >>> WithData.HomaCodeFunction.decodeN 2 (HDX [1,2,3] :: HDataX Int)
-- [6,1,4]
