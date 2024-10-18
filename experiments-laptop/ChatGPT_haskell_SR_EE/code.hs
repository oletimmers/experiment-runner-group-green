{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO.Error (catchIOError)
import Control.Exception (throwIO)
import qualified Data.Text.Encoding as TE
import Data.Char (toLower)

-- Type signature for case-insensitive replacement
replaceCaseInsensitive :: T.Text -> T.Text -> T.Text -> T.Text
replaceCaseInsensitive target replacement = T.replace (T.toLower target) replacement . T.toLower

-- Helper function for reading the file safely
readFileSafe :: FilePath -> IO T.Text
readFileSafe filePath = catchIOError (TIO.readFile filePath) handler
  where
    handler _ = return T.empty  -- Return empty text if an error occurs

-- Function to process the file and replace the target string with the replacement
processFile :: FilePath -> T.Text -> T.Text -> IO ()
processFile filePath target replacement = do
    content <- readFileSafe filePath
    let modifiedContent = replaceCaseInsensitive target replacement content
    TIO.putStrLn modifiedContent  -- Print modified content without overwriting the original file

-- Entry point of the program
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, target, replacement] -> do
            if T.null (T.pack target)
            then putStrLn "Target string cannot be empty."
            else processFile filePath (T.pack target) (T.pack replacement)
        _ -> putStrLn "Usage: program <filename> <target> <replacement>"

