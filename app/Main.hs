{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad (when)
import Functions.Generate (generate)
import System.Console.Docopt (Arguments, Docopt, Option, argument, command, docoptFile, getArgOrExitWith, isPresent, parseArgsOrExit)
import System.Environment (getArgs)

patterns :: Docopt
patterns = [docoptFile|Usage.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` command "generate") $ do
    file <- args `getArgOrExit` argument "file"
    _ <- generate file
    return ()