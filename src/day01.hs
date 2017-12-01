module Main where
import Data.Char (digitToInt)
main :: IO ()
main = do
  cs <- getLine
  let ns = (fmap (digitToInt) cs)
  -- zip list with itself but one of the copies has extra first element in the beginning.
  let pairs = (zip ns ((last ns): ns))
  let s = (sum . (fmap fst) . (filter (\x-> fst x == snd x))) pairs
  putStrLn $ show s
