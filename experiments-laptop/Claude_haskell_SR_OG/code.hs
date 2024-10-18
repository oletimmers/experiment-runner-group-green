import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Control.Exception (catch, IOException)

-- Helper function for case-insensitive replacement
replaceInsensitive :: T.Text -> T.Text -> T.Text -> T.Text
replaceInsensitive target replacement txt =
    T.unwords $ map replaceWord $ T.words txt
  where
    replaceWord word =
        if T.toLower word == T.toLower target
            then replacement
            else word

-- Function to handle file reading errors
readFileContent :: FilePath -> IO T.Text
readFileContent filePath = catch (T.readFile filePath) handleError
  where
    handleError :: IOException -> IO T.Text
    handleError _ = return T.empty

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, target, replacement] -> do
            content <- readFileContent filePath
            let modifiedContent = replaceInsensitive (T.pack target) (T.pack replacement) content
            T.putStrLn modifiedContent
        _ -> putStrLn "Usage: program <file_path> <target_string> <replacement_string>"