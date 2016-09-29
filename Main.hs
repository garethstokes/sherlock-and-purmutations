module Main where

import Control.Monad (replicateM)
import Data.List.Split (splitOn)

testData :: (Num a) => (a, a)
testData = (2, 3)

-- | calculates the unique permutations for the input pair
uniquePermutations :: (Num a, Eq a, Integral a) => (a, a) -> a
uniquePermutations (0, _) = 0
uniquePermutations (_, 0) = 0
uniquePermutations (zeros, ones) = floor $ enumerator / denumerator
   where n = zeros + ones
         enumerator  = fromIntegral $ factoral n
         denumerator = fromIntegral $ factoral zeros * factoral ones

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
