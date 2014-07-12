module Ch18.UglyStack where

import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig { cfgMaxDepth :: Int }
               deriving (Show)

data AppState = AppState { stDeepestReached :: Int }
              deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
        let config = AppConfig maxDepth
            state = AppState 0
        in runStateT (runReaderT k config) state
