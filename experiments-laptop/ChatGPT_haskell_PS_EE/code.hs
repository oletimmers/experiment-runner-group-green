import qualified Data.Map as Map

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
  -- Test cases
  let nums = [2,7,11,15]
  let target = 9
  let result = twoSum nums target
  putStrLn $ show result