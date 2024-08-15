module HomaCode (
    code
  , decode

  , codeN
  , decodeN

  , negData
  , sumData
  , difData

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
-- sub a b = (10 + a - b) `mod` 10



type HData = [Int]

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
decode n = fst $ foldr (\e (r, a) -> (r <> [add e a], add e a)) ([], 0) n

_decode :: HData -> HData
_decode n = fst $ foldr decSum ([], 0) n
  where
    decSum e (r, a) = (r <> [a'], a')
      where
        a' = add e a


codeN :: Int -> HData -> HData
codeN n arr = iterate code arr !! n

decodeN :: Int -> HData -> HData
decodeN n arr = iterate decode arr !! n

-- Loops


-- Matrix map