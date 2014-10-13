{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Ch15.SupplyInstance where

import Control.Applicative ( Applicative(..) )
import Control.Monad ( liftM )

import Ch15.SupplyClass


newtype Reader e a = R { runReader :: e -> a }

instance Functor (Reader e) where
    fmap f (R r) = R $ f . r

instance Applicative (Reader e) where
    pure    = return
    f <*> v = R $ \r -> runReader f r $ runReader v r

instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k  = R $ \r -> runReader (k (runReader m r)) r

ask :: Reader e e
ask = R id

newtype MySupply e a = MySupply { runMySupply :: Reader e a }
                       deriving (Applicative, Functor, Monad)

instance MonadSupply e (MySupply e) where
    next = MySupply (Just `liftM` ask)

xy :: (Num s, MonadSupply s m) => m s
xy = do
    Just x <- next
    Just y <- next
    return (x * y)

runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply
