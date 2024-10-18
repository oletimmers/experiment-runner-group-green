import Data.Maybe (fromJust)
import qualified Data.Map as Map

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

-- Main function for testing
main :: IO ()
main = do
  -- Test cases
  let nums = [2,7,11,15]
  let target = 9
  let result = twoSum nums target
  putStrLn $ show result