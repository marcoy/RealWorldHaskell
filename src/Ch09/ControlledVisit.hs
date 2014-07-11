module Ch09.ControlledVisit where

import Control.Monad(liftM, forM)
import Control.Exception( bracket
                        , handle
                        , SomeException)
import Data.Time.Clock(UTCTime(..))
import System.Directory ( Permissions(..)
                        , doesDirectoryExist
                        , getDirectoryContents
                        , getModificationTime
                        , getPermissions
                        , searchable)
import System.FilePath((</>))
import System.IO( openFile
                , hClose
                , hFileSize
                , IOMode(..))

data Info = Info {
            infoPath :: FilePath
          , infoPerms :: Maybe Permissions
          , infoSize :: Maybe Integer
          , infoModTime :: Maybe UTCTime
          } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle handler (Just `liftM` act)
    where handler :: SomeException -> IO (Maybe a)
          handler _ = return Nothing
