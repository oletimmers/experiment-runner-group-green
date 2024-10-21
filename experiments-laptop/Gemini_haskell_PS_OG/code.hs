import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.IO (readFile)
import Text.Read (readMaybe)
import Control.Monad (when)

-- Function to find two indices that sum up to the target
twoSum :: [Int] -> Int -> (Int, Int)
twoSum nums target = head [(i,j) | i <- [0..(length nums - 1)], j <- [(i+1)..(length nums - 1)], nums !! i + nums !! j == target]

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