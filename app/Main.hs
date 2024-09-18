module Main (main) where

import WithData.HomaCode

main :: IO ()
main  = do
  print $ toTape ([1,2,3] :: [Int])
