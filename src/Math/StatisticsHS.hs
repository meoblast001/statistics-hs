-- Copyright (C) 2014 Braden Walters
-- This file is licensed under the MIT Expat License. See LICENSE.txt.

module Math.StatisticsHS
( mean
, trimmedMean
, trimmedMean1
, median
, sampleRange
, sampleVariance
, sampleVariance'
, standardDeviation
, fiveNumberSummary
, lowerFourth
, upperFourth
, fourthSpread
, outliers
) where

import qualified Data.List as List

mean :: (Fractional a) => [a] -> a
mean list = (foldl1 (+) list) / (fromIntegral (length list))

trimmedMean :: (RealFrac b, Fractional a) => [a] -> b -> a
trimmedMean list percent = let list_length = fromIntegral $ length list
                               trimNum = round $ list_length * percent
                               trimmedRight = drop trimNum $ reverse list
                               trimmedLeft = drop trimNum $ reverse list
                           in mean trimmedLeft

trimmedMean1 :: (Fractional a) => [a] -> a
trimmedMean1 list = mean $ reverse (drop 1 (reverse (drop 1 list)))

median :: (Ord a) => [a] -> a
median list = let sorted_list = List.sort list
                  sorted_list_length = fromIntegral $ length sorted_list
                  index = (round $ sorted_list_length / 2) - 1
              in sorted_list !! index

sampleRange :: (Num a, Ord a) => [a] -> a
sampleRange list = let sorted_list = List.sort list
                       fst = head sorted_list
                       lst = last sorted_list
                   in lst - fst

sampleVariance :: (Fractional a, Ord a) => [a] -> a
sampleVariance list = let list_length = fromIntegral $ length list
                      in foldl1 (+) $ map (\x -> (x - mean list) ^ 2 / (list_length - 1)) list

sampleVariance' :: (Fractional a, Ord a) => [a] -> a
sampleVariance' list = let left = foldl1 (+) $ map (\x -> x ^ 2) list
                           right = (foldl1 (+) list) ^ 2
                           list_length = fromIntegral $ length list
                       in (left - (right / list_length)) / (list_length - 1)

standardDeviation :: (Floating a, Fractional a, Ord a) => [a] -> a
standardDeviation list = sqrt $ sampleVariance list

fiveNumberSummary :: (Fractional a, Ord a) => [a] -> (a, a, a, a, a)
fiveNumberSummary list = let sorted = List.sort list
                             m = median sorted
                             lower = takeWhile (/= m) sorted
                             upper = tail $ dropWhile (/= m) sorted
                         in (head sorted, median lower, m, median upper, last sorted)

lowerFourth :: (Fractional a, Ord a) => [a] -> a
lowerFourth list = let sorted = List.sort list
                       m = median sorted
                   in median $ takeWhile (/= m) sorted

upperFourth :: (Fractional a, Ord a) => [a] -> a
upperFourth list = let sorted = List.sort list
                       m = median sorted
                   in median $ tail $ dropWhile (/= m) sorted

fourthSpread :: (Fractional a, Ord a) => [a] -> a
fourthSpread list = upperFourth list - lowerFourth list

outliers :: (Fractional a, Ord a) => [a] -> a -> [a]
outliers list strength = let lower = lowerFourth list
                             upper = upperFourth list
                             spread = fourthSpread list
                             low_cutoff = lower - strength * spread
                             high_cutoff = upper + strength * spread
                         in filter (\x -> x < low_cutoff && x > high_cutoff) list
