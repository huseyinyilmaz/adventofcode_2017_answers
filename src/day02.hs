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

minMaxSum :: [[Int]] -> Int
minMaxSum lss = sum $ fmap (\x -> maximum x - minimum x) lss

main :: IO ()
main = do
  ls <- readNumbers
  (putStrLn . show . minMaxSum) ls
