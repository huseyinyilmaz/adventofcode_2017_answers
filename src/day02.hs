module Main where
import Data.Char (digitToInt)
import Data.List (sort)

readNumbers :: IO [[Int]]
readNumbers = do
  content <- getContents
  return $ fmap (sort . readLine) (lines content)
  where
    readLine :: String -> [Int]
    readLine = (fmap read) . words

getListDiff x = last x - head x

getSum :: [[Int]] -> ([Int] -> Int) -> Int
getSum lss f = sum $ fmap f lss

minMaxSum :: [[Int]] -> Int
minMaxSum lss = getSum lss getListDiff

main :: IO ()
main = do
  ls <- readNumbers
  (putStrLn . show . minMaxSum) ls
