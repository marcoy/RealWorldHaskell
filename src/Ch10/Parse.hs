module Ch10.Parse where

import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)

data ParseState = ParseState { string :: L.ByteString
                             , offset :: Int64
                             } deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

newtype Parse a = Parse { runParse :: ParseState -> Either String (a, ParseState) }
