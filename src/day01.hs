module Main where
import Data.Char (digitToInt)

rotate :: [a] -> Int -> [a]
rotate l n = drop n l ++ take n l

readNumbers :: IO [Int]
readNumbers = getLine >>= return . (fmap digitToInt)

calculateSum :: Int -> [Int] -> Int
calculateSum n l = (sum . (fmap fst) . (filter firstEqSecond) . listToPairs) l
  where
    listToPairs l = (zip l (rotate l n))
    firstEqSecond (a , b) = a == b

printSum :: Int -> [Int] -> IO()
printSum n l = (putStrLn . show . (calculateSum n)) l

main :: IO ()
main = do
  ns <- readNumbers
  printSum 1 ns
  printSum (length ns `div` 2) ns
