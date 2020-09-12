module Main where

import           BinaryTree    (BinaryTree (..), valuesPreOrder)
import           Control.Monad (guard)
import           Data.Maybe    (fromJust, isJust)

{-

@onelharrison - [Twitter | Medium]

data BinaryTree a = Empty
    | Leaf a
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Show, Eq)

Description
-----------
Write a function that deserializes a preorder string-encoded binary tree.

(String -> BinaryTree Char) . (BinaryTree Char -> String) = BinaryTree Char -> BinaryTree Char
         ??                 . valuesPreOrder

The following defines the set of valid values in the encoded tree.

p node
  - parent node
  - internal node
  - root node if root node has at least 1 child node

w node
  - leaf node
  - left child of a p-node

b node
  - leaf node
  - right child of a p-node

Examples
--------
Input: ""
Output: Empty

Input: "w"
Output: Leaf 'w'
Explanation

        w

Input: "b"
Output: Leaf 'b'
Explanation:

        b

Input: "p"
Output: Invalid input
Explanation: root node without any children cannot be a p-node

Input: "pw"
Output: Node (Leaf 'w') 'p' Empty
Explanation:

        p
       /
      w

Input: "pb"
Output: Node Empty 'p' (Leaf 'b')
Explanation:

        p
         \
          b

Input: "pp"
Output: Invalid input
Explanation: p-node cannot be leaf node

Input: "pwpwb"
Output: Node (Leaf 'w') 'p' (Node (Leaf 'w') 'p' (Leaf 'b'))
Explanation:

        p
       / \
      w   p
         / \
        w   b

Input: "ppwb"
Output: Node (Node (Leaf 'w') 'p' (Leaf 'b')) 'p' Empty
Explanation:

        p
       /
      p
     / \
    w   b

Input: "pwppwb"
Output: Node (Leaf 'w') 'p' (Node (Node (Leaf 'w') 'p' (Leaf 'b)) 'p' Empty)
Explanation:

        p
       / \
      w   p
         /
        p
       / \
      w   b

data BinaryTree a = Empty
    | Leaf a
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Show, Eq)
-}
preOrderToTree :: String -> Maybe (BinaryTree Char)
preOrderToTree "" = Just Empty
preOrderToTree "w" = Just (Leaf 'w')
preOrderToTree "b" = Just (Leaf 'b')
preOrderToTree ('p':'w':xs) = do
  lst <- preOrderToTree "w"
  rst <- preOrderToTree xs
  return (Node lst 'p' rst)
preOrderToTree ('p':'b':xs) =
  if null xs
    then do
      rst <- preOrderToTree "b"
      return (Node Empty 'p' rst)
    else Nothing
preOrderToTree ('p':'p':xs) = do
  lst <- preOrderToTree ('p' : xs)
  return (Node lst 'p' Empty)
preOrderToTree _ = Nothing

main :: IO ()
main = do
  str <- getLine
  let tree = preOrderToTree . head . lines $ str
  print tree
  guard (isJust tree)
  print . valuesPreOrder $ fromJust tree
