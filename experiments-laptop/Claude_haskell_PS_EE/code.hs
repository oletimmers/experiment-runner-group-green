import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import System.IO (readFile)
import Text.Read (readMaybe)
import Control.Monad (when)

-- Function to find two indices that sum up to the target
twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target = go Map.empty nums 0
  where
    go seen [] _ = error "No solution found"
    go seen (x:xs) i =
      case Map.lookup (target - x) seen of
        Just j -> (j, i)
        Nothing -> go (Map.insert x i seen) xs (i + 1)
            
-- Main function for testing
main :: IO ()
main = do
  -- Read the contents of the file "input.txt"
  content <- readFile "input.txt"
  
  -- Split the content into lines
  let (targetLine:numsLine:_) = lines content
  
  -- Read the target
  let maybeTarget = readMaybe targetLine :: Maybe Int
  
  -- Read the numbers from the second line
  let maybeNums = mapM (readMaybe :: String -> Maybe Int) (words numsLine)
  
  case (maybeTarget, maybeNums) of
    (Just target, Just nums) -> do
      let result = twoSum nums target
      putStrLn $ show result
    _ -> putStrLn "Error: Invalid input format in input.txt."