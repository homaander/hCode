module Main (main) where

import qualified Code.HomaCode as HC
import Code.HomaCodeData

main :: IO ()
main  = do
  print $ toTape ([1,2,3] :: [Int])
