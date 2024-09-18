module Main (main) where

import Code.HomaCode

main :: IO ()
main  = do
  print $ toTape ([1,2,3] :: [Int])
