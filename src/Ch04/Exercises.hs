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
