-- Function to find two indices that sum up to the target
twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target = head [(i,j) | i <- [0..(length nums - 1)], j <- [(i+1)..(length nums - 1)], nums !! i + nums !! j == target]

-- Main function for testing
main :: IO ()
main = do
  -- Test cases
  let nums = [2,7,11,15]
  let target = 9
  let result = twoSum nums target
  putStrLn $ show result