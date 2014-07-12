module Ch18.CountEntriesT where

import Ch18.CountEntries (listDirectory)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, execWriterT, runWriterT, tell)

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
        contents <- liftIO . listDirectory $ path
        tell [(path, length contents)]
        forM_ contents $ \name -> do
            let newName = path </> name
            isDir <- liftIO . doesDirectoryExist $ newName
            when isDir $ countEntries newName

-- take 4 `liftM` execWriterT (countEntries "..")
