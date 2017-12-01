module Main where
import Data.Char (digitToInt)

calculateSum :: String -> String
calculateSum = show . sum . (fmap fst) . (filter firstEqSecond) . listToPairs . (fmap digitToInt)
  where
    listToPairs l = (zip l ((last l): l))
    firstEqSecond (a , b) = a == b

main :: IO ()
main = fmap calculateSum getLine >>= putStrLn
