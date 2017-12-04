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

readNumber :: IO Int
readNumber = getLine >>= (return . read)

lastCircleSize :: Int -> Int
lastCircleSize n = head $ dropWhile (\ x -> x*x < n) [1,3..]

findDistance :: Int -> Int
findDistance n =
  distanceFromMiddle + halfSide
  where
    largeSize = (lastCircleSize n)
    smallSize = (largeSize - 2)
    side = largeSize -1
    halfSide = side `div` 2
    lastPoint = largeSize * largeSize
    firstPoint = (smallSize * smallSize)
    index = n - firstPoint
    locationOnSide = index `mod` side
    distanceFromMiddle = abs $ locationOnSide - halfSide


getCoordinateCircle :: Int -> [(Int, Int)]
getCoordinateCircle n =
  [(n, y) | y <- [n-1, n-2 .. (-n)]] ++
  [(x, -n) | x <- [n-1, n-2 .. (-n)]] ++
  [(-n, y)| y <- [-(n-1)..n]] ++
  [(x, n) | x <- [-(n-1)..n]]


-- 0,0 is not on the list because it is on default state
coordinateList = [ coordinate |
                   circleId<-[1..],
                   coordinate<-getCoordinateCircle circleId]

getValue :: Coordinate -> AppM Int
getValue (x,y) = do
  (limit, mapping) <- get
  let value = ((Map.findWithDefault 0 (x-1,y-1) mapping)+
               (Map.findWithDefault 0 (x-1,y) mapping)+
               (Map.findWithDefault 0 (x-1,y+1) mapping)+
               (Map.findWithDefault 0 (x,y+1) mapping)+
               (Map.findWithDefault 0 (x+1,y+1) mapping)+
               (Map.findWithDefault 0 (x+1,y) mapping)+
               (Map.findWithDefault 0 (x+1,y-1) mapping)+
               (Map.findWithDefault 0 (x,y-1) mapping)
              )
  put (limit, (Map.insert (x,y) value mapping))
  return value

values :: [Coordinate] -> AppM [(Coordinate, Int)]
values [] = return []
values (c:cs) = do
  (limit, _)<- get
  value <- getValue c
  if value > limit
    then return [(c, value)]
    else do rest <- values cs
            return ((c, value): rest)

getLatestValue :: Int -> AppM Int
getLatestValue n = do
  coordinates <- values coordinateList
  return $ last $ fmap snd coordinates

main :: IO ()
main = do
  n <- readNumber
  (putStrLn . show) n
  (putStrLn . show) (findDistance n)
  (putStrLn . show) (evalState (getLatestValue n) (n, (Map.singleton (0,0) 1)))
