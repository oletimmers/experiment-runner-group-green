import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

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
  -- Test cases
  let nums = [2,7,11,15]
  let target = 9
  let result = twoSum nums target
  putStrLn $ show result