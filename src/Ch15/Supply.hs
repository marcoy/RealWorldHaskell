{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ch15.Supply
        (
          Supply
        , next
        , runSupply
        )
    where


import Control.Applicative ( Applicative )
import Control.Monad.State( State
                          , get
                          , put
                          , runState)


newtype Supply s a = S (State [s] a)
                     deriving (Applicative, Functor, Monad)

-- instance Monad (Supply s) where
--     return  = S . return
--     s >>= k = S (unwrapS s >>= unwrapS . k)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of
                  []     -> return Nothing
                  (x:xs) -> do put xs
                               return (Just x)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

showTwo :: (Show s) => Supply s String
showTwo = do
    a <- next
    b <- next
    return (show "a: " ++ show a ++ ", b: " ++ show b)
