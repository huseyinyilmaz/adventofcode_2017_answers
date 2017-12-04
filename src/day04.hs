module Main where
import Data.Char (digitToInt)
import Data.List (sort)
import Control.Applicative((<|>))
import Data.Maybe (fromJust)
import Control.Monad.State.Lazy
-- import Control.Monad.Trans.State
import qualified Data.Map.Strict as Map

type Coordinate = (Int, Int)

type CoordinateMapping = Map.Map Coordinate Int

type AppM = State (Int, CoordinateMapping)


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
