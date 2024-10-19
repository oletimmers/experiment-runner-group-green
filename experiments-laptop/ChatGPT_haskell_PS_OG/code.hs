import Data.Maybe (fromJust)
import qualified Data.Map as Map
import System.IO (readFile)
import Text.Read (readMaybe)
import Control.Monad (when)

-- Function to find two indices that sum up to the target
twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target = fromJust $ findIndices nums target Map.empty 0
  where
    findIndices :: [Int] -> Int -> Map.Map Int Int -> Int -> Maybe (Int, Int)
    findIndices [] _ _ _ = Nothing
    findIndices (x:xs) target map index =
      let complement = target - x
      in case Map.lookup complement map of
           Just complementIndex -> Just (complementIndex, index)
           Nothing -> findIndices xs target (Map.insert x index map) (index + 1)

-- Main function to read input from input.txt and run twoSum logic
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

