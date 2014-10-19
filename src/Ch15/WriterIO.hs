{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ch15.WriterIO where

import Control.Applicative ( Applicative )
import Control.Monad ( Monad )
import Control.Monad.Writer ( MonadWriter, Writer, runWriter )
import System.IO ( IOMode )

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
           deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
                     deriving (Applicative, Functor, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW
