module Ch03.Exercises where

import Data.List (sortBy)

-- Write a function that computes the number of elements in a list. To
-- test it, ensure that it gives the same answers as the standard length
-- function.
myLen :: [a] -> Int
myLen (x:xs) = 1 + myLen xs
myLen []     = 0


-- Write a function that computes the mean of a list, i.e. the sum of all
-- elements in the list divided by its length. (You may need to use the
-- fromIntegral function to convert the length of the list from an integer
-- into a floating point number.)
myMean :: (Fractional a) => [a] -> a
myMean [] = 0
myMean xs = sum xs / fromIntegral (length xs)


-- Turn a list into a palindrome, i.e. it should read the same both
-- backwards and forwards. For example, given the list [1,2,3], your
-- function should return [1,2,3,3,2,1].
toPalindrome :: [a] -> [a]
toPalindrome [] = []
toPalindrome xs = xs ++ rev xs
                  where rev []     = []
                        rev (x:xs) = rev xs ++ [x]


-- Write a function that determines whether its input list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs


-- Create a function that sorts a list of lists based on the length of each
-- sublist. (You may want to look at the sortBy function from the Data.List
-- module.)
sublistSort :: [[a]] -> [[a]]
sublistSort = sortBy (\xs ys -> compare (length xs) (length ys))

myintersperse :: a -> [[a]] -> [a]
myintersperse _ [] = []
myintersperse _ [x] = x
myintersperse s (x:xs) = x ++ [s] ++ myintersperse s xs
