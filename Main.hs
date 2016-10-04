{- Sherlock and Permutations
 - -------------------------
 -
 - A solution for the problem found here
 - https://www.hackerrank.com/challenges/sherlock-and-permutations
 -
 - A slight twist on the typical permutations problem in that it will
 - remove duplicate permutations and only return the number of count
 - of uniques
 -
 - special help to this article for information about modulo 
 - mathematics
 -
 - https://codeaccepted.wordpress.com/2014/02/15/output-the-answer-modulo-109-7/
 -}

module Main where

import Control.Monad (replicateM)

testData :: (Num a) => (a, a)
testData = (2, 3)

-- | calculates the unique permutations for the input pair
uniquePermutations :: (Num a, Eq a, Integral a) => (a, a) -> a
uniquePermutations (zeros, ones) 
  -- only deal with positive inputs
  | zeros < 1 = 0 
  | ones  < 1 = 0

  -- unique starting with 1
  | ones == 1 = 1

  -- unque permutations: n! / duplicates!
  | otherwise = (permutations * littleFermat duplicates prime) `mod` prime
     where n = (zeros + ones - 1) `mod` prime
           permutations = factoral n `mod` prime
           duplicates   = factoral zeros `mod` prime * factoral (ones - 1) `mod` prime `mod` prime

prime :: (Integral a) => a
prime = 1000000007

-- | fast power function
-- complexity of O(log p) much faster than using ^
fastPow :: (Integral a) => a -> a -> a -> a
fastPow base 0 _ = 1
fastPow base 1 _ = base
fastPow base pow m 
  | even pow = mod ((fastPow base (div pow 2) m) ^ 2) m
  | odd  pow = mod ((fastPow base (div (pow-1) 2) m) ^ 2 * base) m

-- | fermats little theorem 
-- p must be a prime number larger than a
littleFermat :: (Integral a) => a -> a -> a
littleFermat a p = fastPow a (p - 2) p

-- | factoral helper method
factoral :: (Num a, Integral a, Eq a) => a -> a
factoral 1 = 1
factoral x = x * factoral (x-1) `mod` prime

-- | parse string input "2 3" to be the pair (2, 3)
strToPair :: String -> (Integer, Integer)
strToPair input = (a, b)
  where [a, b] = map read $ words input :: [Integer]

main = do
  numberOfLines <- getLine
  lines <- replicateM (read numberOfLines) getLine
  putStr $ unlines $ map (show . uniquePermutations . strToPair) lines
