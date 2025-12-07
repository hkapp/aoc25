import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State (State, get, put, evalState)
import Data.Map (Map)
import qualified Data.Map as Map

process = part2
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/07.example.txt"
      else
        readFile "./data/07.input.txt"

    let res = process $ parse textInput
    print $ res

parse :: String -> (Pos, [Splitters])
parse input =
  let
    (startLine : splitterLines) = lines input
  in
    (findStart startLine, map (buildSplitters . findSplitters) splitterLines)

type Pos = Int

findStart :: String -> Pos
findStart = fst . fromJust . find (((==) 'S') . snd) . zipWithIndex

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip (integers 0)

integers :: Int -> [Int]
integers n = n : (integers (n+1))

findSplitters :: String -> [Pos]
findSplitters = map fst . filter (((==) '^') . snd) . zipWithIndex

type Splitters = Set Pos

buildSplitters :: [Pos] -> Splitters
buildSplitters = Set.fromList

part1 :: (Pos, [Splitters]) -> Int
part1 (start, world) = sum $ map fst $ scanl ((flip splitAndCount) . snd) (0, singleBeam start) world

type Beams = Set Pos

singleBeam :: Pos -> Beams
singleBeam = Set.singleton

applySplitters :: Splitters -> Beams -> Beams
applySplitters splitters currBeams =
  let
    applyRec newBeams (b : remBeams) =
      let
        newNewBeams =
          if Set.member b splitters then
            Set.insert (b-1) $ Set.insert (b+1) newBeams
          else
            Set.insert b newBeams
      in
        applyRec newNewBeams remBeams

    applyRec newBeams [] = newBeams
  in
    applyRec Set.empty (Set.toList currBeams)

splitAndCount :: Splitters -> Beams -> (Int, Beams)
splitAndCount splitters beams =
  let
    splitCount = length $ Set.intersection splitters beams
    newBeams = applySplitters splitters beams
  in
    (splitCount, newBeams)

part2 (startPos, world) = evalState (countCached startPos world) Map.empty

type Level = Int

type Cache = Map (Pos, Level) Int

countCached :: Pos -> [Splitters] -> State Cache Int

countCached _ [] = return 1

countCached pos splitters =
  do
    cache <- get
    let level = length splitters
    case Map.lookup (pos, level) cache of
      Just count -> return count
      Nothing ->
        do
          put cache
          let newDims = splitOne (head splitters) pos
          subCounts <- sequence $ map (\newPos -> countCached newPos (tail splitters)) newDims
          let totDimCount = sum subCounts
          newCache <- get
          put $ Map.insert (pos, level) totDimCount newCache
          return totDimCount

splitOne :: Splitters -> Pos -> [Pos]
splitOne splitters pos =
  if Set.member pos splitters then
    [pos - 1, pos + 1]
  else
    [pos]
