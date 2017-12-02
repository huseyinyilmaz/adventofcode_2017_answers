module Main where
import Data.Char (digitToInt)

main :: IO ()
main = do
  content <- getContents
  let ls = ((fmap ((fmap read) . words) (lines content)))
  let s = (sum $ fmap (\x -> maximum x - minimum x) ls)
  putStrLn $ show s
