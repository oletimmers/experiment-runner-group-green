{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.IO.Error (catchIOError)
import Control.Exception (IOException)

-- Type Signature for the case-insensitive replace function
caseInsensitiveReplace :: T.Text -> T.Text -> T.Text -> T.Text
caseInsensitiveReplace target replacement content =
    T.replace (T.toLower target) replacement (T.toLower content)

-- Read file content with error handling
readFileContent :: FilePath -> IO T.Text
readFileContent filePath = catchIOError (TIO.readFile filePath) handleError
  where
    handleError :: IOException -> IO T.Text
    handleError _ = return T.empty

-- Main function
main :: IO ()
main = do
    -- Get command line arguments (filename, target, replacement)
    args <- getArgs
    case args of
        [filePath, target, replacement] -> do
            -- Read the content of the file
            content <- readFileContent filePath
            if T.null content
                then putStrLn "File is empty or could not be read."
                else do
                    -- Perform case-insensitive replacement
                    let modifiedContent = caseInsensitiveReplace (T.pack target) (T.pack replacement) content
                    -- Print the modified content
                    TIO.putStrLn modifiedContent
        _ -> putStrLn "Usage: program <filename> <target> <replacement>"