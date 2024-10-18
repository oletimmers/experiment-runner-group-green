import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)

-- Case-insensitive replacement function
replaceCaseInsensitive :: T.Text -> T.Text -> T.Text -> T.Text
replaceCaseInsensitive target replacement text = 
  T.replace (T.toLower target) (T.toLower replacement) (T.toLower text)

-- Function to read file content, handling errors gracefully
readFileContent :: FilePath -> IO T.Text
readFileContent filePath = catchIOError (TIO.readFile filePath) (handleError filePath)

-- Error handler for file reading
handleError :: FilePath -> IOError -> IO T.Text
handleError filePath error
  | isDoesNotExistError error = return T.empty -- Return empty Text if file not found
  | otherwise = return  $ T.pack $ "Error reading file '" ++ filePath ++ "': " ++ show error

-- Main function to process the file
processFile :: FilePath -> T.Text -> T.Text -> IO ()
processFile filePath target replacement = do
  content <- readFileContent filePath
  let modifiedContent = 
        if T.null target 
        then content 
        else replaceCaseInsensitive target replacement content
  TIO.putStrLn modifiedContent

-- Entry point of the program
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename, target, replacement] -> processFile filename (T.pack target) (T.pack replacement)
    _ -> putStrLn "Usage: ./replace-text <filename> <target> <replacement>"

-- Helper function to catch IO errors and return T.Text
catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError action handler = catch action (handler)