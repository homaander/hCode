module HomaCodeData (
    HNums(..)
  , Math(..)

  , HDataX(..)
  , HData5(..)

  , HTape (..)
) where

data HNums = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9
  deriving (Show, Enum)

newtype HDataX a = HDX [a]

instance (Show a) => Show (HDataX a) where
  show (HDX a) = show a 


data HData5 a = HD5 a a a a a
  deriving (Show)

type TapeOffset = Int
type TapeLength = Int
data HTape h = HT h TapeOffset TapeLength 



class (Show a) => Math a where
  -- instance
  add :: a -> a -> a
  neg :: a -> a

  -- default
  sub :: a -> a -> a
  sub a b = add a (neg b)

  (^+^) :: a -> a -> a
  (^+^) = add

  (^-^) :: a -> a -> a
  (^-^) = sub


-- Math instance
instance Math Int where
  add a b = (a + b) `mod` 10
  neg n   = (10 - n) `mod` 10

instance Math HNums where
  add a b = toEnum $ add (fromEnum a) (fromEnum b)
  neg n   = toEnum $ neg (fromEnum n)

instance Math a => Math (HDataX a) where
  add (HDX a) (HDX b) = HDX $ zipWith add a b
  neg (HDX a) = HDX $ map neg a

instance Math a => Math (HData5 a) where
  add (HD5 a0 a1 a2 a3 a4) (HD5 b0 b1 b2 b3 b4) = HD5 (a0 ^+^ b0) (a1 ^+^ b1) (a2 ^+^ b2) (a3 ^+^ b3) (a4 ^+^ b4)
  neg (HD5 a0 a1 a2 a3 a4) = HD5 (neg a0) (neg a1) (neg a2) (neg a3) (neg a4)