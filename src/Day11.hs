import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

process = part2
inputFile = "./data/11.input.txt"

main =
  do
    textInput <- readFile inputFile
    let res = process $ parse textInput
    print $ res

type Graph = Map Node [Node]

parse :: String -> Graph
parse = Map.insert "out" [] . Map.fromList . map (parseLine . words) . lines

type Node = String

parseLine :: [String] -> (Node, [Node])
parseLine (x:xs) = (takeWhile ((/=) ':') x, xs)

part1 g = countPaths g "out" "you"

countPaths :: Graph -> Node -> Node -> Int
countPaths g end start =
  if end == start
    then 1
    else sum $ map (countPaths g end) (debugLookup g start)

debugLookup :: (Show k, Ord k) => Map k a -> k -> a
debugLookup m k = fromMaybe (error ("Missing key " ++ (show k))) $ Map.lookup k m

-- svr -> dac -> fft -> out
-- or
-- svr -> fft -> dac -> out
part2 g = countPathsVia g "out" "fft" "dac" "svr" + countPathsVia g "out" "dac" "fft" "svr"

countPathsVia :: Graph -> Node -> Node -> Node -> Node -> Int
countPathsVia g end secondStop firstStop start =
  let
    -- start to firstStop without secondStop
    c1 = countPathsWithout g secondStop firstStop start
    -- firstStop to secondStop
    c2 = countPaths g secondStop firstStop
    -- second stop to end without first stop
    c3 = countPathsWithout g firstStop end secondStop
  in
    c1 * c2 * c3

countPathsWithout :: Graph -> Node -> Node -> Node -> Int
countPathsWithout g avoid end start =
  let
    grm = rmNode avoid g
  in
    countPaths grm end start

rmNode :: Node -> Graph -> Graph
rmNode n = fmap (filter ((/=) n))
