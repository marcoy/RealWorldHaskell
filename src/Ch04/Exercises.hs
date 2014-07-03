module Ch04.Exercises where

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')

-- Use a fold (choosing the appropriate fold will make your code much
-- simpler) to rewrite and improve upon the asInt function.
asIntFold :: String -> Int
asIntFold s = foldl' step 0 s
              where step acc x = acc * 10 + digitToInt x

asIntFold' :: String -> Int
asIntFold' ('-':xs) = (- asIntFold' xs)
asIntFold' s = foldl' step 0 s
               where step acc x = acc * 10 + digitToInt x


-- The asIntFold function uses error, so its callers cannot handle errors.
-- Rewrite it to fix this problem
type ErrorMessage = String
asIntEither :: String -> Either ErrorMessage Int
asIntEither s = foldl' step (Right 0) s
                where step (Left errMsg) _ = Left errMsg
                      step (Right acc) x
                        | not (isDigit x) = Left $ (show x) ++ " is not a digit"
                        | otherwise       = Right (acc * 10 + digitToInt x)


-- Write your own definition of concat using foldr
foldrConcat :: [[a]] -> [a]
foldrConcat = foldr (++) []


-- Write your own definition of the standard takeWhile function, first
-- using explicit recursion, then foldr
recTakeWhile :: (a -> Bool) -> [a] -> [a]
recTakeWhile _ []     = []
recTakeWhile p (x:xs) = if p x
                        then x:recTakeWhile p xs
                        else []

foldrTakeWhile :: (a -> Bool) -> [a] -> [a]
foldrTakeWhile _ [] = []
foldrTakeWhile p xs = foldr step [] xs
                      where step x acc
                                | p x       = x:acc
                                | otherwise = []


-- Use ghci to load the Data.List module and figure out what groupBy does,
-- then write your own implementation using a fold
foldGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
foldGroupBy p xs = combine $ foldr step ([],[]) xs
                   where step x ([],acc)     = ([x],acc)
                         step x (sub,acc)
                            | p x (head sub) = (x:sub,acc)
                            | otherwise      = ([x],sub:acc)
                         combine (sub,acc) = sub:acc

foldGroupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
foldGroupBy' p xs = foldr step [] xs
                    where step x []           = [[x]]
                          step x acc@(ys:yss)
                              | p x (head ys) = (x:ys):yss
                              | otherwise     = [x]:acc
