module Main where
import Data.Char (digitToInt)
main :: IO ()
main = fmap calculateSum getLine >>= putStrLn
  where
    listToPairs l = (zip l ((last l): l))
    firstEqSecond (a , b) = a == b
    calculateSum = show . sum . (fmap fst) . (filter firstEqSecond) . listToPairs . (fmap digitToInt)
