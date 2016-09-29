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

module Main where

import Control.Monad (replicateM)
import Data.List.Split (splitOn)

testData :: (Num a) => (a, a)
testData = (2, 3)

-- | calculates the unique permutations for the input pair
uniquePermutations :: (Num a, Eq a, Integral a) => (a, a) -> a
uniquePermutations (zeros, ones) 
  | zeros < 1 = 0
  | ones < 1  = 0
  | otherwise = floor $ permutations / duplicates
     where n = zeros + ones
           permutations = fromIntegral $ factoral n
           duplicates   = fromIntegral $ factoral zeros * factoral ones

-- | factoral helper method
factoral :: (Num a, Integral a, Eq a) => a -> a
factoral 1 = 1
factoral x = x * factoral (x-1)

-- | parse string input "2 3" to be the pair (2, 3)
strToPair :: String -> (Integer, Integer)
strToPair input = (a, b)
  where [a, b] = map read $ splitOn " " input :: [Integer]

main = do
  numberOfLines <- getLine
  lines <- replicateM (read numberOfLines) getLine
  putStr $ unlines $ map (show . uniquePermutations . strToPair) lines
