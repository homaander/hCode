module Code.HomaCode (
    Math(..)
  , Code(..)
  , CodeRecurse(..)
  , Tapes(..)
  , HData(..)
  , HDataInfo(..)

  , HNums16(..)
  , HTape (..)
) where

import Code.HomaCodeData
import Code.HomaCodeClasses

-- >>>  Code.HomaCodeClasses.toHDataN 6 1234 :: [Int]
-- [0,0,1,2,3,4]

-- >>> [H12,H02,H13,H04,H06]
-- >>> Code.HomaCodeClasses.code [H12,H02,H13,H04,H06]
-- [C,2,D,4,6]
-- [2,7,B,6,C]

-- >>> [H12,H02,H13,H03,H14,H04]
-- >>> Code.HomaCodeClasses.codeN 66 [H12,H02,H13,H03,H14,H04]
-- [C,2,D,3,E,4]
-- [5,8,4,C,3,2]

-- >>> Code.HomaCodeClasses.decode ([1,2,5] :: [Int])
-- [5,7,8]

-- >>> Code.HomaCodeClasses.decodeN 2 ([1,2,3] :: [Int])
-- [6,1,4]

-- >>> Code.HomaCodeClasses.toTape ([0,1,2,3,4] :: [Int])
-- HTape {tapeId = [0,0,0,0,1], tapeOffset = 17811, tapeAntiOffset = 6400, tapeLength = 24211}
