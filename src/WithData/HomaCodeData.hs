module WithData.HomaCodeData (
    HNums16(..)
  , HDataX(..)
  , HTape (..)
) where

data HNums16 =  H0 |  H1 |  H2 |  H3
             |  H4 |  H5 |  H6 |  H7
             |  H8 |  H9 | H10 | H11
             | H12 | H13 | H14 | H15
  deriving (Enum, Bounded)

instance Show HNums16 where
  show  H0 = "0"
  show  H1 = "1"
  show  H2 = "2"
  show  H3 = "3"
  show  H4 = "4"
  show  H5 = "5"
  show  H6 = "6"
  show  H7 = "7"
  show  H8 = "8"
  show  H9 = "9"
  show H10 = "A"
  show H11 = "B"
  show H12 = "C"
  show H13 = "D"
  show H14 = "E"
  show H15 = "F"


newtype HDataX a = HDX [a]


instance Show a => Show (HDataX a) where
  show (HDX arr) = show arr 


data HTape h = HTape {
    tapeId :: h
  , tapeOffset :: Int
  , tapeLength :: Int
}