{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Control.Exception (catch, IOException)

-- Helper function for case-insensitive replacement
replaceInsensitive :: T.Text -> T.Text -> T.Text -> T.Text
replaceInsensitive target replacement input =
    T.replace (T.toCaseFold target) replacement input

-- Function to handle file reading errors
readFileContents :: FilePath -> IO T.Text
readFileContents filePath = catch (T.readFile filePath) handleError
  where
    handleError :: IOException -> IO T.Text
    handleError _ = return T.empty

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, target, replacement] -> do
            contents <- readFileContents filePath
            let modifiedContents = replaceInsensitive (T.pack target) (T.pack replacement) contents
            T.putStrLn modifiedContents
        _ -> putStrLn "Usage: program <file_path> <target_string> <replacement_string>"