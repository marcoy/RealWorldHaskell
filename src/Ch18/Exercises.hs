{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Ch18.Exercises where

import Control.Monad (liftM)
import Control.Monad.State (State, MonadState, evalState, get , put)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)

newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

instance (Monad m) => Monad (EitherT a m) where
        return  = EitherT . return . Right
        e >>= k = EitherT $ do
                    v <- runEitherT e
                    case v of
                        (Left v')  -> return $ Left v'
                        (Right v') -> runEitherT $ k v'

instance MonadTrans (EitherT a) where
        -- Given an inner monad, m, lift it into EitherT
        lift = EitherT . (liftM Right)

instance (MonadState s m) => MonadState s (EitherT left m) where
        get = lift get
        put = lift . put


data ParseState = ParseState { string :: L.ByteString
                             , offset :: Int64
                             } deriving (Show)

newtype Parse a = P { runP :: EitherT String (State ParseState) a }
                  deriving (Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Either String a
evalParse m s = evalState (runEitherT (runP m)) (ParseState s 0)


-- A simple test
simpleParserEither :: Parse String
simpleParserEither = return "OK"
testParse = evalParse simpleParserEither L.empty
