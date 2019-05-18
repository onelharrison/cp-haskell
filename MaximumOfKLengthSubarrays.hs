{--

@onelharrison [Twitter | Medium]

Description
-----------
Write a function that takes an array of integers of size N and a number k and
computes the maximum values for each k-length subarray.

Constraints
-----------
 * 1 <= k <= N

Example
-------
Input: arr = [10, 5, 2, 7, 8, 7], k = 3
Output: [10, 7, 8, 8]

Example Explanation
-------------------
* 10 = max [10, 5, 2]
* 7  = max [5,  2, 7]
* 8  = max [2,  7, 8]
* 8  = max [7,  8, 7]

--}

import Data.List

kSubMax :: [Int] -> Int -> [Int]
kSubMax xs k = map maximum
               $ map (take k)
               $ filter ((>=k) . length)
               $ tails xs

