module Compiler.Tokenizer (tokenize) where

import Data.Char (isSpace)
import Utils (isDelim)

padDelims :: String -> String
padDelims = concatMap (\c -> if isDelim [c] then [' ', c, ' '] else [c])

tokenizeWord :: [String] -> String -> [String]
tokenizeWord acc rest = case dropWhile isSpace rest of
  "" -> reverse acc
  rest' -> case rest' of
    '"' : rest'' -> case break (== '"') rest'' of
      (_quotedContent, "") -> reverse (rest' : acc) -- no closing quote was found
      (quotedContent, '"' : restAfterQuote) -> tokenizeWord ("\"" : quotedContent : "\"" : acc) restAfterQuote
      _ -> error "" -- should never be able to happen
    _ -> case break isSpace rest' of
      (word, "") -> reverse (word : acc)
      (word, restAfterSpace) -> tokenizeWord (word : acc) restAfterSpace

tokenize :: String -> [String]
tokenize code = tokenizeWord [] $ padDelims code