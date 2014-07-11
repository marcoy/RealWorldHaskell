module Ch11.QCBasics where

import Data.List( (\\)
                , minimum
                , sort)
import Test.QuickCheck( (==>)
                      , Property(..))


qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ rhs
    where lhs = filter (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: (Ord a) => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum :: (Ord a) => [a] -> Property
prop_minimum xs = not (null xs) ==> head (qsort xs) == minimum xs

-- the output is ordered
prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

-- the output is a permutation of the input
prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

prop_sort_model xs = sort xs == qsort xs
