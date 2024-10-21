import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.IO (readFile)
import Text.Read (readMaybe)
import Control.Monad (when)

-- Function to find two indices that sum up to the target
twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target = go nums Map.empty 0
  where
    -- Helper function to iterate through the list
    go :: [Int] -> Map.Map Int Int -> Int -> (Int, Int)
    go [] _ _ = error "No solution found"
    go (x:xs) map index = 
      let complement = target - x
      in case Map.lookup complement map of
           Just complementIndex -> (complementIndex, index)
           Nothing -> go xs (Map.insert x index map) (index + 1)

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