module Utils (isDelim, throw, snakeCasify, escapeInvalidChars) where

import Data.Char (isAlphaNum)
import GHC.Base (ord)

moduleSeparator :: Char
moduleSeparator = '/'

isValidIndentifierChar :: Char -> Bool
isValidIndentifierChar char = isAlphaNum char || char == '_'

isModuleSeparator :: Char -> Bool
isModuleSeparator char = char == moduleSeparator

delims :: [String]
delims = ["[", "]", "(", ")", ";"]

isDelim :: String -> Bool
isDelim c = c `elem` delims

replaceAll :: (Eq a) => a -> a -> [a] -> [a]
replaceAll a b = map (\x -> if a == x then b else x)

snakeCasify :: String -> String
snakeCasify = replaceAll '-' '_'

escapeInvalidChars :: String -> String
escapeInvalidChars input =
  if all (== '_') input
    then input
    else
      concatMap
        ( \c ->
            if isModuleSeparator c
              then "."
              else
                if isValidIndentifierChar c
                  then [c]
                  else "__" ++ show (ord c) ++ "__"
        )
        input

throw :: (Show a) => a -> b
throw err = error $ show err