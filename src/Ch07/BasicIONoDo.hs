module Ch07.BasicIONoDo where

main = putStrLn "Greeings!" >>
       getLine >>= \inpStr ->
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
