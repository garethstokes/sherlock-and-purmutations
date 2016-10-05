{- Sherlock and Permutations
 - -------------------------
 -
 - A solution for the problem found here
 - https://www.hackerrank.com/challenges/sherlock-and-permutations
 -
 - A slight twist on the typical permutations problem in that it will
 - remove duplicate permutations and only return the number of count
 - of uniques
 -}

import Control.Monad (replicateM)

-- | calculates the unique permutations for the input pair
uniquePermutations :: (Num a, Eq a, Integral a) => (a, a) -> a
uniquePermutations (zeros, ones) 
  -- only deal with positive inputs
  | zeros < 1 = 0 
  | ones  < 1 = 0

  -- unique starting with 1
  | ones == 1 = 1

  -- unque permutations: n! / duplicates!
  | otherwise = (permutations `div` duplicates) `mod` prime
     where n = zeros + ones - 1
           permutations = factoral n 
           duplicates   = factoral zeros * factoral (ones - 1)

prime :: (Integral a) => a
prime = 1000000007

-- | factoral helper method
factoral :: (Num a, Integral a, Eq a) => a -> a
factoral 1 = 1
factoral x = x * factoral (x-1)

-- | parse string input "2 3" to be the pair (2, 3)
strToPair :: String -> (Integer, Integer)
strToPair input = (a, b)
  where [a, b] = map read $ words input :: [Integer]

main = do
  numberOfLines <- getLine
  lines <- replicateM (read numberOfLines) getLine
  putStr $ unlines $ map (show . uniquePermutations . strToPair) lines
