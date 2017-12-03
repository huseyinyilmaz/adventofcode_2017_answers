module Main where
import Data.Char (digitToInt)
import Data.List (sort)
import Control.Applicative((<|>))
import Data.Maybe (fromJust)

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

divide :: Int -> [Int] -> Maybe Int
divide n [] = Nothing
divide n (a:l)
  | (a `mod` n) == 0 = Just (a `div` n)
  | otherwise = (divide n l) <|> (divide a l)

minMaxSum :: [[Int]] -> Int
minMaxSum lss = getSum lss getListDiff

divisionSum :: [[Int]] -> Int
divisionSum lss = getSum lss (\(n:l) -> fromJust (divide n l))

main :: IO ()
main = do
  ls <- readNumbers
  (putStrLn . show) ls
  (putStrLn . show . minMaxSum) ls
  (putStrLn . show . divisionSum) ls
