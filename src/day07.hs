-- input
-- tqefb (40)
-- lhrml (164) -> ecblhee, sdjshz

module Main where
import Data.Char (digitToInt)
import Data.Set
--import Data.Vector
import Data.List (dropWhile, last, isInfixOf)
import Data.Map

data Program = Program {
  getName :: String,
  getNumber :: Int,
  getChildren :: [String]
  } deriving Show


parseProgram :: [Char] -> Program
parseProgram raw =
  case children of
    [] -> Program name num []
    otherwise -> Program name num (tail children)
  where
    name:rawNumber:rawChildren = words raw
    num = ((read.init.tail) rawNumber)
    removeComma x = if last x == ',' then init x else x
    children = fmap removeComma rawChildren

readInput = fmap ((fmap parseProgram).lines) getContents

makeTree :: Map String Program -> String -> Program
makeTree m name = undefined

findRoot :: [Program] -> Program
findRoot ps = head $ Prelude.filter (\x -> not $ getName x `elem` cs) ps
  where
    cs = (Data.Set.fromList [ c | p<-ps, c<-getChildren p]):: Set String


-- find the program that has children and is not children of anybody else.
part1 :: [Program]-> IO()
part1 ps = do
  putStrLn "Part1"
  putStrLn "-----"
  print $ findRoot ps
  putStrLn "-----"

main :: IO()
main = do
  ps <- readInput
  putStrLn "-----"
  part1 ps
