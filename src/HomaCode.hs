module HomaCode (
    code
  , decode

  , codeN
  , decodeN

  , toHData
  , fromHData

  , negData
  , sumData
  , difData

  , recurseCode
  , findCount
  , findLoop
  , findLoopArr

  , add
  , sub
  , neg
  ) where

add :: Int -> Int -> Int
add a b = (a + b) `mod` 10

neg :: Int -> Int
neg n = (10 - n) `mod` 10

sub :: Int -> Int -> Int
sub a b = add a (neg b)



type HData = [Int]

toHData :: Int -> HData
toHData n = map (\pow -> (n `div` pow) `mod` 10) powArr
  where
    len = length $ show n
    powLen = reverse [0 .. len - 1]
    powArr = map (10 ^) powLen

fromHData :: HData -> Int
fromHData hd = sum $ zipWith (*) hd powArr
  where
    len = length hd
    powLen = reverse [0 .. len - 1]
    powArr = map (10 ^) powLen


negData :: HData -> HData
negData = map neg

sumData :: HData -> HData -> HData
sumData = zipWith add

difData :: HData -> HData -> HData
difData = zipWith sub


code :: HData -> HData
code n = map (uncurry sub) pairs
  where
    pairs = reverse $ zip n (0 : n)

decode :: HData -> HData
decode n = fst $ foldr decSum ([], 0) n
  where
    decSum e (r, a) = (r <> [add e a], add e a)

-- decode n = fst $ foldr (\e (r, a) -> (r <> [add e a], add e a)) ([], 0) n

codeN :: Int -> HData -> HData
codeN n hd = iterate code hd !! n

decodeN :: Int -> HData -> HData
decodeN n hd = iterate decode hd !! n

-- Loops

recurseCode :: HData -> HData
recurseCode hd = iterate code hd !! fromHData hd

findLoop :: HData -> Int
findLoop hd = foldr 
  (\he n -> if he == hd then 1 else n + 1) 0
  $ iterate code (code hd)

findCount :: HData -> HData -> Int
findCount ihd hd = foldr 
  (\he n -> if he == hd then 1 else n + 1) 0
  $ iterate code (code ihd)

findLoopArr :: HData -> [HData]
findLoopArr hd = hd : foldr 
  (\he n -> if he == hd then [] else [he] <> n) []
  (iterate code $ code hd)

-- findLoopGroups :: HData -> HData -> Int
-- findLoopGroups hda hdb = map (\e -> )
--   where
--     d1 = fromHData hda
--     d2 = fromHData hdb
--     lp = [d1..d2]

-- Matrix map