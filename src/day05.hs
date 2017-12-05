module Main where
import Data.Array

steps :: Array Int Int -> Int -> Int -> [Int]
steps a len i
  | i<0 || i>=len = []
  | otherwise = i : steps (a//[(i, v+1)]) len (v+i)
    where
      v = a!i

steps2 :: Array Int Int -> Int -> Int -> [Int]
steps2 a len i
  | i<0 || i>=len = []
  | otherwise = i : steps2 (a//[(i, newV)]) len (v+i)
    where
      v = a!i
      newV = if v>2 then v-1 else v+1

main :: IO ()
main = do
  content <- getContents
  let is = ((fmap read) . lines) content :: [Int]
  let len = length is
  let a = array (0, len-1) (zip [0..] is)
  print $ length $ steps a len 0
  print $ length $ steps2 a len 0
