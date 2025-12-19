import Data.Map (Map, (!))
import qualified Data.Map as Map

process = part2
inputFile = "./data/11.input.txt"

main =
  do
    textInput <- readFile inputFile
    let res = process $ parse textInput
    print $ res

type Graph = Map Node [Node]

parse :: String -> Graph
parse = Map.fromList . map (parseLine . words) . lines

type Node = String

parseLine :: [String] -> (Node, [Node])
parseLine (x:xs) = (takeWhile ((/=) ':') x, xs)

part1 g =
  let
    start = "you"
  in
    length $ search g start

search :: Graph -> Node -> [[Node]]
search g "out" = [[]]
search g start = map ((:) start) $ (g ! start) >>= search g

part2 g = length $ filter (elem "dac") $ filter (elem "fft") $ search g "svr"
