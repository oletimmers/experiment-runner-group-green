import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- Function to find two indices that sum up to the target
twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target = go nums IntMap.empty 0
  where
    go [] _ _ = error "No solution found!" -- Guaranteed to have a solution
    go (x:xs) seen i =
      case IntMap.lookup (target - x) seen of
        Just j -> (j, i)
        Nothing -> go xs (IntMap.insert x i seen) (i + 1)

-- Main function for testing
main :: IO ()
main = do
  -- Test cases
  let nums = [2,7,11,15]
  let target = 9
  let result = twoSum nums target
  putStrLn $ show result