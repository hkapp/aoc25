import Data.Tuple (swap)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (State, modify, get, execState)
import Data.Foldable (traverse_)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (find, sortOn)
import Data.Ord (Down(Down))

process = part2
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/09.example.txt"
      else
        readFile "./data/09.input.txt"
    let res = process $ parse textInput
    print $ res

type Point = (Int, Int)

parse :: String -> [Point]
parse = map read . map (\s -> "(" ++ s ++ ")") . lines

part1 = maximum . map surface . combinations

combinations :: [a] -> [(a, a)]
combinations (x:xs) = (map ((,) x) xs) ++ (combinations xs)
combinations [] = []

type Rectangle = (Point, Point)

{-
  x-->
 y
 |
 v
-}
surface :: Rectangle -> Int
surface ((x1, y1), (x2, y2)) =
  let
    width = abs (x2 - x1) + 1
    height = abs (y2 - y1) + 1
  in
    width * height

part2 points =
  let
    outline = buildOutline points  -- 2.5s
    corner = topLeftCorner points  -- 1.6s
    rightDown = (1, 1)
    shellCorner = shellStart corner rightDown outline  -- 2.14s
    shell = execState (walkShell outline shellCorner) Set.empty  -- 10.8s

    -- count: 122760
    allRects = possibleRectangles points
    sortedRects = sortOn (Down . surface) allRects  -- 1.801s

    -- Note: the 1000th sorted rect has surface 4536405203
    -- The shell has size 587618
    isValidRect = not . overlaps (Set.toList shell)
    Just bestRect = find isValidRect sortedRects  -- 10 -> 10.7s, 100 -> 10.96s, 1000 -> 14.7s, 10000 -> 51s
  in
    surface bestRect

possibleRectangles :: [Point] -> [Rectangle]
possibleRectangles = combinations

buildOutline :: [Point] -> Set Point
buildOutline points =
  let
    pointLoop = (last points) : points
    outline = overlappingPairs pointLoop >>= uncurry drawLine
  in
    Set.fromList outline

drawLine :: Point -> Point -> [Point]
drawLine (x1, y1) (x2, y2) =
  if x1 == x2 then
    map ((,) x1) $ range (min y1 y2) (max y1 y2)
  else if y1 == y2 then
    map swap $ map ((,) y1) $ range (min x1 x2) (max x1 x2)
  else
    error $ "Can't draw the line " ++ (show (x1, y1)) ++ " -> " ++ (show (x2, y2))

range :: Int -> Int -> [Int]
range low high =
  if low == high then
    [high]
  else
    low : (range (low + 1) high)

overlappingPairs :: [a] -> [(a, a)]
overlappingPairs (x:y:ys) = (x, y) : (overlappingPairs (y:ys))
overlappingPairs _ = []

topLeftCorner :: [Point] -> Point
topLeftCorner points =
  case unzip points of
    (xs, ys) -> (minimum xs - 1, minimum ys - 1)

shellStart :: Point -> Point -> Set Point -> Point
shellStart startingPoint direction outline =
  if partOfShell outline startingPoint then
    startingPoint
  else
    shellStart (move startingPoint direction) direction outline

debug :: Int -> a -> a
debug n v =
  if n `mod` 1000 == 0 then
    unsafePerformIO $ print n >> return v
  else
    v

move :: Point -> Point -> Point
move (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

neighbours :: Point -> [Point]
neighbours (x, y) =
  [
    (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1),
    (x + 1, y + 1),
    (x + 1, y - 1),
    (x - 1, y + 1),
    (x - 1, y - 1)
  ]

walkShell :: Set Point -> Point -> State (Set Point) ()
walkShell outline currPos =
  do
    prevShell <- get
    if Set.member currPos prevShell then
      -- We already visited this position
      return ()
    else
      do
        modify $ Set.insert currPos
        let visitNext = filter (partOfShell outline) $ neighbours currPos
        traverse_ (walkShell outline) visitNext

partOfShell :: Set Point -> Point -> Bool
partOfShell outline point =
  (not $ Set.member point outline) && (any (\p -> Set.member p outline) $ neighbours point)

-- Note: we go over the points of the outline as we expect that number to be smaller
-- than what a rectangle could be
overlaps :: [Point] -> Rectangle -> Bool
overlaps points rectangle = any (containedIn rectangle) points

containedIn :: Rectangle -> Point -> Bool
containedIn ((rx1, ry1), (rx2, ry2)) (x, y) =
  inRange (min rx1 rx2, max rx1 rx2) x && inRange (min ry1 ry2, max ry1 ry2) y

inRange :: (Ord a) => (a, a) -> a -> Bool
inRange (low, high) x = x >= low && x <= high