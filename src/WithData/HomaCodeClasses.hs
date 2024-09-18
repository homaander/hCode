module WithData.HomaCodeClasses (
    Math(..)
  , Code(..)
  , CodeRecurse(..)
  , Tapes(..)
  , DataSet(..)
) where

import WithData.HomaCodeData ( HTape(..) )
import Data.Maybe ( fromMaybe )

class DataSet box where
  fromHData :: Enum a => box a -> Int
  toHData   :: Enum a => Int -> box a
  toHDataN  :: (Math a, Enum a) => Int -> Int -> box a


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

class DataSet box => CodeRecurse box where
  codeRecurse :: (Enum a, Code (box a)) => box a -> box a
  nextNCode   :: (Enum a, Code (box a)) => Int -> box a -> [box a]

class (Ord a, Code a) => Tapes a where
  -- instance
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


