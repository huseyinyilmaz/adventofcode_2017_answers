module Main where
import Data.List (sort)

readWords :: IO [[String]]
readWords = do
  content <- getContents
  return $ fmap (sort.words) (lines content)


isUnique :: [String] -> Bool
isUnique ws = not $ any equalTuple pairs
  where
    pairs = zip ws (tail ws)
    equalTuple x = fst x == snd x

hasAnagram :: [String] -> Bool
hasAnagram ws = not $ isUnique sortedWs
  where
    sortedWs = sort $ fmap sort ws


main :: IO ()
main = do
  wss <- readWords
  (putStrLn . show) $ length $ filter isUnique wss
  (putStrLn . show) $ length $ filter (not . hasAnagram) wss
