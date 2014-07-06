module Ch08.GlobRegex
        (
          globToRegex
        , matchesGlob
        ) where

import Text.Regex.Posix ((=~))


type GlobError = String


matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' ""             = ""
globToRegex' ('*':cs)       = ".*" ++ globToRegex' cs
globToRegex' ('?':cs)       = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"
globToRegex' (c:cs)         = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"


--
-- Exercise
--
globToRegexEither :: String -> Either GlobError String
globToRegexEither cs = case globToRegexEither' cs of
                           (Right r) -> Right $ '^' : r ++ "$"
                           err       -> err

globToRegexEither' :: String -> Either GlobError String
globToRegexEither' ""             = Right ""
globToRegexEither' ('*':cs)       = Right $ ".*" ++ globToRegex' cs
globToRegexEither' ('?':cs)       = Right $ '.' : globToRegex' cs
globToRegexEither' ('[':'!':c:cs) = Right $ "[^" ++ c : charClass cs
globToRegexEither' ('[':c:cs)     = Right $ '['  :  c : charClass cs
globToRegexEither' ('[':_)        = Left "unterminated character class"
globToRegexEither' (c:cs)         = Right $ escape c ++ globToRegex' cs
