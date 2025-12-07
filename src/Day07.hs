import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Set (Set)

process = part1
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