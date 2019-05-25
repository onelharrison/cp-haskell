{-

@onelharrison - [Twitter | Medium]

Description
-----------
Write a function that takes a string ss and an integer k, and finds the
length of the longest substring T that contains at most k distinct
characters.

Constraints
-----------
* 1 <= k

Examples
--------
Input: ss = "eceba", k = 2
Output: 3

Input: ss = "abcba", k = 2
Output: 3

Input: ss = "aa", k = 1
Output: 2

Example Explanations
--------------------
* T is "ece"
* T is "bcb"
* T is "aa"

-}

import Data.List
import Data.Function

maxSubstrLen :: Int -> [Char] -> Int
maxSubstrLen k = length
  . maximumBy (compare `on` length)
  . filter ((<= k) . length . nub)
  . concatMap inits
  . tails

