import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Control.Exception (catch, IOException)
import Data.Text.Internal.Search (indices)

-- Function to perform case-insensitive replacement
replaceCaseInsensitive :: T.Text -> T.Text -> T.Text -> T.Text
replaceCaseInsensitive target replacement text =
  let
    targetLower = T.toLower target
    replacementLower = T.toLower replacement
    textLower = T.toLower text
    occurrences = indices targetLower textLower
  in
    foldr (\i acc -> T.take i acc <> replacementLower <> T.drop (i + T.length targetLower) acc) textLower occurrences

-- Function to read file content, handling potential errors
readFileContent :: FilePath -> IO T.Text
readFileContent filePath = catch (TIO.readFile filePath) handler
  where
    handler :: IOException -> IO T.Text
    handler _ = return T.empty

-- Main function to process file content
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
    [filePath, target, replacement] -> processFile filePath (T.pack target) (T.pack replacement)
    _ -> putStrLn "Usage: ./replace-text <file_path> <target_string> <replacement_string>"