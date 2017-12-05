module Main where
import Data.Vector.Primitive

steps :: Vector Int -> Int -> Int -> [Int]
steps a len i
  | i<0 || i>=len = []
  | otherwise = i : steps (a//[(i, v+1)]) len (v+i)
    where
      v = a!i

steps2 :: Vector Int -> Int -> Int -> [Int]
steps2 v len i
  | i<0 || i>=len = []
  | otherwise = i : steps2 (v//[(i, newV)]) len (val+i)
    where
      val = v!i
      newV = if val>2 then val-1 else val+1

main :: IO ()
main = do
  content <- getContents
  let is = ((fmap read) . lines) content :: [Int]
  let v = fromList is
  let len = Data.Vector.Primitive.length v
  print $ Prelude.length $ steps v len 0
  print $ Prelude.length $ steps2 v len 0
