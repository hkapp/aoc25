import Data.List (sortOn, find, uncons, partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (State, get, put, runState, evalState)
import Data.Ord (Down(Down))

process = part1
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/08.example.txt"
      else
        readFile "./data/08.input.txt"
    let res = process $ parse textInput
    print $ res

type Point = [Int]

parse :: String -> [Point]
parse = map read . map (\s -> "[" ++ s ++ "]") . lines

part1 points =
  let
    sortedPairs = sortOn (uncurry distance) $ allPairs points
    initialConns = map Set.singleton points
    repeatCount = if example then 10 else 1000
    repeatedProcess = sequence $ take repeatCount $ repeat pickAndConnect
    (output, (remPairs, finalConns)) = runState repeatedProcess (sortedPairs, initialConns)
  in
    product $ take 3 $ sortOn Down $ map length finalConns

distance :: Point -> Point -> Double
distance p1 p2 = sqrt $ fromIntegral $ sum $ map square $ map (\(a, b) -> abs (a - b)) $ zip p1 p2

square :: (Num a) => a -> a
square x = x * x

allPairs :: [a] -> [(a, a)]
allPairs (x:xs) = (map ((,) x) xs) ++ (allPairs xs)
allPairs [] = []

type Pair = (Point, Point)
type SortedPairs = [Pair]
type Connections = [Circuit]
type Circuit = Set Point

pickAndConnect :: State (SortedPairs, Connections) (Pair, Bool)
pickAndConnect =
  do
    (candidates, currConns) <- get
    let picked:remaining = candidates

    let work = not $ alreadyConnected currConns picked
    let newConns = if work then
                    connect currConns picked
                  else
                    currConns

    put (remaining, newConns)
    return (picked, work)

alreadyConnected :: Connections -> Pair -> Bool
alreadyConnected allConns (x, y) =
  case find (Set.member x) allConns of
    Just xCircuit ->
      -- x is part of a circuit
      -- x and y may be in the same circuit
      Set.member y xCircuit
    Nothing ->
      -- x is not part of any circuit
      -- x and y cannot be in the same
      False

connect :: Connections -> Pair -> Connections
connect prevConns (x, y) =
  let
    (xCircuit:[], xRem) = partition (Set.member x) prevConns
    (yCircuit:[], remaining) = partition (Set.member y) xRem
  in
    (Set.union xCircuit yCircuit) : remaining
