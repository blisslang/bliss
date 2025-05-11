module Functions.Generate (generate) where

import Compiler.Categorizer (categorize)
import Compiler.Emitter (emit)
import Compiler.Tokenizer (tokenize)
import Constants (buildDir)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, exeExtension)

generate :: String -> IO (String, String)
generate inputFileName = do
  contents <- readFile inputFileName

  let lastDotIdx =
        length inputFileName
          - 1
          - fromMaybe
            0
            (elemIndex '.' $ reverse inputFileName)
  let (inputFileNameWithoutExt, _) = splitAt lastDotIdx inputFileName
  let outputFileNameWithoutExt = buildDir ++ "/" ++ inputFileNameWithoutExt

  let lastSlashIdx =
        length outputFileNameWithoutExt
          - 1
          - fromMaybe
            0
            (elemIndex '/' $ reverse outputFileNameWithoutExt)
  let (outputDir, _) = splitAt lastSlashIdx outputFileNameWithoutExt

  let irOutputFileName = outputFileNameWithoutExt ++ ".re"
  let exeOutputFileName = outputFileNameWithoutExt ++ exeExtension

  putStrLn $ "==> Generating: " ++ inputFileName ++ " -> " ++ irOutputFileName

  putStrLn "   🔹 Tokenizing source"

  let tokens = tokenize contents

  putStrLn "   🔹 Parsing tokens"

  let ast = categorize tokens

  putStrLn "   🔹 Emitting IR"

  let ir = emit ast

  createDirectoryIfMissing True outputDir
  writeFile irOutputFileName ir

  putStrLn $ "   ✅️ Generated IR: " ++ irOutputFileName

  -- putStrLn "TOKENS:"
  -- print tokens
  -- putStrLn "AST:"
  -- print ast
  -- putStrLn "IR:"
  -- putStrLn ir

  return (irOutputFileName, exeOutputFileName)
