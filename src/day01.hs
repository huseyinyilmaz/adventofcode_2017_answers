module Main where
import Data.Char (digitToInt)
import Data.Array

readNumbers :: IO [Int]
readNumbers = getLine >>= return . (fmap digitToInt)

calculateSum :: [Int] -> Int
calculateSum = sum . (fmap fst) . (filter firstEqSecond) . listToPairs
  where
    listToPairs l = (zip l ((last l): l))
    firstEqSecond (a , b) = a == b

printSum :: [Int] -> IO()
printSum = putStrLn . show . calculateSum


halfSum :: [Int] -> Int
halfSum ns = sumIndexedList indexedList
  where
    size = length ns
    indexedList = zip [0..size] ns
    a = array (0, size-1) indexedList
    nextIndex i = (i + size `div` 2) `mod` size
    isHalfMatch (i, n)= n == a ! (nextIndex i)
    sumIndexedList = sum . (fmap snd) . (filter isHalfMatch)

printMatchHalf :: [Int] -> IO()
printMatchHalf = putStrLn . show . halfSum

main :: IO ()
main = do
  ns <- readNumbers
  printSum ns
  printMatchHalf ns
