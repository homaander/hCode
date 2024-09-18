module Code.HomaCodeData (
    HNums16(..)
  , HTape (..)
) where

data HNums16 = H00 | H01 | H02 | H03
             | H04 | H05 | H06 | H07
             | H08 | H09 | H10 | H11
             | H12 | H13 | H14 | H15
  deriving (Enum, Eq, Ord, Bounded)

-- HNums16 to hex
instance Show HNums16 where
  show H00 = "0"
  show H01 = "1"
  show H02 = "2"
  show H03 = "3"
  show H04 = "4"
  show H05 = "5"
  show H06 = "6"
  show H07 = "7"
  show H08 = "8"
  show H09 = "9"
  show H10 = "A"
  show H11 = "B"
  show H12 = "C"
  show H13 = "D"
  show H14 = "E"
  show H15 = "F"


data HTape hdata = HTape {
    tapeId         :: hdata
  , tapeOffset     :: Int
  , tapeAntiOffset :: Int
  , tapeLength     :: Int
}
  deriving Show