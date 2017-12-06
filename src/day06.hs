module Main where
import Data.Char (digitToInt)
import Data.Set
import Data.Vector
import Data.List (dropWhile, last)

readNumbers :: IO [Int]
readNumbers = getLine >>= return . (fmap read) . words

distribute :: Vector Int -> Int -> Int -> Vector Int
distribute ns idx 0 = ns
distribute ns idx val = distribute ns' idx' val'
  where
    ns' = ns // [(idx, ((ns!idx) + 1))]
    idx' = (idx + 1) `mod` (Data.Vector.length ns)
    val' = (val-1)

countDistribution :: Vector Int -> Set (Vector Int) -> [Vector Int]
countDistribution ns seen =
  if
    ns `member` seen
  then
    [ns]
  else
    ns : (countDistribution (distribute newNs idx maxVal) (insert ns seen))
    where
      maxIdx = (Data.Vector.maxIndex ns)
      maxVal = ns!maxIdx
      newNs = ns // [(maxIdx, 0)]
      idx = (maxIdx+1) `mod` (Data.Vector.length ns)


findLoop ns = Data.List.dropWhile (\x-> x /= lastElement) ns
  where
    lastElement = Data.List.last ns

main :: IO ()
main = do
  ns <- readNumbers
  print ns
  print $ ((\x -> x-1) . Prelude.length) $ countDistribution (Data.Vector.fromList ns) Data.Set.empty
  print $ ((\x -> x-1). Prelude.length) $ findLoop $ countDistribution (Data.Vector.fromList ns) Data.Set.empty
