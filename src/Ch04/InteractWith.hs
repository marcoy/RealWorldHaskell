module Main where

import System.Environment (getArgs)
import Ch04.SplitLines (fixLines)

interactWith function inputFile outputFile = do
        input <- readFile inputFile
        writeFile outputFile (function input)

main :: IO ()
main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"
          myFunction = fixLines
