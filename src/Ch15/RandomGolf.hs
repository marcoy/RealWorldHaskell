module Ch15.RandomGolf where

import Control.Arrow ( first )
import System.Random ( Random, getStdRandom, randoms, split )

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)
