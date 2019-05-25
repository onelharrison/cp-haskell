{--

@onelharrison [Twitter | Medium]

Description
-----------
Write a function that takes an array nums of N integers, and returns
an array products where products[i] is the product of all the elements
of nums excluding nums[i].

Solve it without using division.

Constraints
-----------
 * N > 1

Example
-------
Input: nums = [1, 2, 3, 4]
Output: products = [24, 12, 8, 6]

Example Explanation
-------------------
 i  | result 
--------------------
 0  | 2 * 3 * 4 = 24
 1  | 1 * 3 * 4 = 12
 2  | 1 * 2 * 4 = 8
 3  | 1 * 2 * 3 = 6

--}

products :: [Int] -> [Int]
products xs = zipWith (*) forwardProducts backwardProducts
   where forwardProducts  = initProductScanl xs
         backwardProducts = (reverse . initProductScanl . reverse) xs
         initProductScanl = init . (scanl (*) 1)

