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
, choose
, expectedValue
, expectedValue1
, randomVariableVariance
, binomialProbability
, negativeBinomialDistribution
, negativeBinomialDistributionMean
, hypergeometricProbability
, hypergeometricMean
, hypergeometricVariance
) where

import qualified Data.List as List

mean :: (Fractional a) => [a] -> a
mean list = (foldl1 (+) list) / (fromIntegral (length list))

trimmedMean :: (RealFrac b, Fractional a) => [a] -> b -> a
trimmedMean list percent =
  let list_length = fromIntegral $ length list
      trimNum = round $ list_length * percent
      trimmedRight = drop trimNum $ reverse list
      trimmedLeft = drop trimNum $ reverse list
  in mean trimmedLeft

trimmedMean1 :: (Fractional a) => [a] -> a
trimmedMean1 list = mean $ reverse (drop 1 (reverse (drop 1 list)))

median :: (Ord a) => [a] -> a
median list =
  let sorted_list = List.sort list
      sorted_list_length = fromIntegral $ length sorted_list
      index = (round $ sorted_list_length / 2) - 1
  in sorted_list !! index

sampleRange :: (Num a, Ord a) => [a] -> a
sampleRange list =
  let sorted_list = List.sort list
      fst = head sorted_list
      lst = last sorted_list
  in lst - fst

sampleVariance :: (Fractional a, Ord a) => [a] -> a
sampleVariance list =
  let list_length = fromIntegral $ length list
  in foldl1 (+) $ map (\x -> (x - mean list) ^ 2 / (list_length - 1)) list

sampleVariance' :: (Fractional a, Ord a) => [a] -> a
sampleVariance' list =
  let left = foldl1 (+) $ map (\x -> x ^ 2) list
      right = (foldl1 (+) list) ^ 2
      list_length = fromIntegral $ length list
  in (left - (right / list_length)) / (list_length - 1)

standardDeviation :: (Floating a, Fractional a, Ord a) => [a] -> a
standardDeviation list = sqrt $ sampleVariance list

fiveNumberSummary :: (Fractional a, Ord a) => [a] -> (a, a, a, a, a)
fiveNumberSummary list =
  let sorted = List.sort list
      m = median sorted
      lower = takeWhile (/= m) sorted
      upper = tail $ dropWhile (/= m) sorted
  in (head sorted, median lower, m, median upper, last sorted)

lowerFourth :: (Fractional a, Ord a) => [a] -> a
lowerFourth list =
  let sorted = List.sort list
      m = median sorted
  in median $ takeWhile (/= m) sorted

upperFourth :: (Fractional a, Ord a) => [a] -> a
upperFourth list =
  let sorted = List.sort list
      m = median sorted
  in median $ tail $ dropWhile (/= m) sorted

fourthSpread :: (Fractional a, Ord a) => [a] -> a
fourthSpread list = upperFourth list - lowerFourth list

outliers :: (Fractional a, Ord a) => [a] -> a -> [a]
outliers list strength =
  let lower = lowerFourth list
      upper = upperFourth list
      spread = fourthSpread list
      low_cutoff = lower - strength * spread
      high_cutoff = upper + strength * spread
  in filter (\x -> x < low_cutoff && x > high_cutoff) list

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

choose :: (Integral a, Fractional b) => a -> a -> b
choose n k =
  let numerator = factorial n
      denominator = factorial (n - k) * factorial k
  in fromIntegral numerator / fromIntegral denominator

permutation :: (Integral a, Fractional b) => a -> a -> b
permutation k n =
  let numerator = factorial n
      denominator = factorial (n - k)
  in fromIntegral numerator / fromIntegral denominator

expectedValue :: (Fractional a) => (a -> a) -> [a] -> [a] -> a
expectedValue f rand_vars probs =
  foldl1 (+) $ zipWith (\r_var prob -> f r_var * prob) rand_vars probs

expectedValue1 :: (Fractional a) => [a] -> [a] -> a
expectedValue1 = expectedValue (\x -> x)

randomVariableVariance :: (Fractional a) => [a] -> [a] -> a
randomVariableVariance rand_vars probs =
  let ev = expectedValue1 rand_vars probs
  in foldl1 (+) $ zipWith (\r_var prob -> (r_var - ev) ^ 2 * prob)
                          rand_vars probs

binomialProbability :: (Integral a, Fractional b) => a -> b -> a -> b
binomialProbability trials prob_success successes =
  (trials `choose` successes) * prob_success ^ successes *
  (1 - prob_success) ^ (trials - successes)

negativeBinomialDistribution :: (Integral a, Fractional b) => a -> b -> a -> b
negativeBinomialDistribution successes success_prob failures =
  ((failures + successes - 1) `choose` (successes - 1)) *
  success_prob ^ successes * (1 - success_prob) ^ failures

negativeBinomialDistributionMean :: (Integral a, Fractional b) => a -> b -> b
negativeBinomialDistributionMean success_total success_prob =
  let success_total_n = fromIntegral success_total
  in (success_total_n * (1 - success_prob)) / success_prob

hypergeometricProbability :: (Integral a, Fractional b) => a -> a -> a -> a -> b
hypergeometricProbability total success_total selected successes =
  let numerator = (success_total `choose` successes) *
                  ((total - success_total) `choose` (selected - successes))
      denominator = total `choose` selected
  in numerator / denominator

hypergeometricMean :: (Integral a, Fractional b) => a -> a -> a -> b
hypergeometricMean total success_total selected =
  let total_n = fromIntegral total
      success_total_n = fromIntegral success_total
      selected_n = fromIntegral selected
  in selected_n * (success_total_n / total_n)

hypergeometricVariance :: (Integral a, Fractional b) => a -> a -> a -> b
hypergeometricVariance total success_total selected =
  let total_n = fromIntegral total
      success_total_n = fromIntegral success_total
      selected_n = fromIntegral selected
  in (success_total_n - selected_n) / (success_total_n - 1) * selected_n *
      success_total_n / total_n * 1 - (success_total_n / total_n)
