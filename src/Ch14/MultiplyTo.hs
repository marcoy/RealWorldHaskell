module Ch14.MultiplyTo where

guarded :: Bool -> [a] -> [a]
guarded True  xs = xs
guarded False _  = []


-- | The empty list are kicked out by concat
--
-- concat [[(1,4)], [(2,2)], [], [], [(4,1)]] 
-- ==> [(1,4),(2,2),(4,1)]
multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
        x <- [1..n]
        y <- [1..n]
        guarded (x * y == n) $
          return (x, y)
