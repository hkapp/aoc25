import Data.Bifunctor (first, second, Bifunctor)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe, listToMaybe, fromMaybe)
import Control.Monad (join)
import Control.Monad.State (State, get, modify, evalState)
import Data.List (find, sortOn, groupBy, nub, sort, foldl')
import System.IO.Unsafe (unsafePerformIO)
import Data.Ord (Down(Down))

process = part1
inputFile = "./data/12.input.txt"

main =
  do
    textInput <- readFile inputFile
    let res = process $ parse textInput
    print $ res

parse = second parseAllRequests . first parseAllGifts . span (not . elem 'x' . head) . splitWhere null . lines

splitWhere     :: (a -> Bool) -> [a] -> [[a]]
splitWhere p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitWhere p s''
                            where (w, s'') = break p s'

type Point = (Int, Int)
type Shape = Set Point
type Catalog = Map Int Shape

parseAllGifts :: [[String]] -> Catalog
parseAllGifts = Map.fromList . zipWithIndex . map (parseOneGift . tail)

parseOneGift :: [String] -> Shape
parseOneGift input =
  let
    asList =
      do
        (y, row) <- zipWithIndex input
        (x, char) <- zipWithIndex row
        case char of
          '#' -> return (x, y)
          _ -> []
  in
    Set.fromList asList

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip (integers 0)

integers :: Int -> [Int]
integers n = n : (integers (n + 1))

parseAllRequests = map parseOneRequest . head

type Area = Point
type ShapeCounts = Map Int Int
type Request = (Area, ShapeCounts)

parseOneRequest :: String -> Request
parseOneRequest input =
  let
    (shapeStr : countsStr) = words input
    rect = mapBoth read $ splitFirst ((==) 'x') $ dropLast shapeStr
    shapeCounts = Map.fromList $ zipWithIndex $ map read countsStr
  in
    (rect, shapeCounts)

dropLast :: [a] -> [a]
dropLast xs = take ((length xs) - 1) xs

splitFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitFirst p xs =
  case span (not . p) xs of
    (ys, zs) -> (ys, tail zs)

mapBoth :: (Bifunctor f) => (a -> b) -> f a a -> f b b
mapBoth f = second f . first f

part1 (catalog, requests) = map (solvable catalog) $ take 1 requests

--solvable :: Catalog -> Request -> Bool
solvable catalog (area, shapeCounts) =
  let
    canvas = emptyCanvas area
    -- We want to first pick the shapes that are repeated many times,
    -- as we can reduce the search space for those
    sortedShapeCounts = sortOn (Down . snd) $ Map.toList shapeCounts
    toPick = (=<<) (\(id, count) -> take count $ repeat $ catalog ! id) sortedShapeCounts
  in
    fit toPick canvas

type Canvas = (Area, Set Point)

emptyCanvas :: Area -> Canvas
emptyCanvas area = (area, Set.empty)

fit :: [Shape] -> Canvas -> Maybe Canvas
fit shapes canvas = evalState (fit2 shapes Map.empty canvas) Set.empty

type History = Map Shape Shape

fit2 :: [Shape] -> History -> Canvas -> State (Set ([Shape], Canvas)) (Maybe Canvas)
fit2 [] _ currCanvas = return $ Just currCanvas
fit2 shapes@(currShape : remShapes) hist currCanvas =
  let
    allVariants =
      do
        rotatedShape <- uniqueRotations currShape
        corner <- canvasFreePoints currCanvas
        let translatedAndRotated = translate rotatedShape corner
        return translatedAndRotated

    placeAndRecord modifiedShape =
      do
        newCanvas <- place currCanvas modifiedShape
        let newHist = Map.insert currShape modifiedShape hist
        let betterCanvas = fillSmallHolesAround modifiedShape newCanvas (length remShapes)
        return $ (newHist, betterCanvas)

    recFit =
      fmap (debugNoLn ((show $ length shapes) ++ " ") safeHead)
      $ fmap (mapMaybe id)
      $ traverse (uncurry $ fit2 remShapes)
      $ filter (canStillFit remShapes . snd)
      $ mapMaybe placeAndRecord
      $ dropWhile (not . trulyNew hist currShape)
      $ sort allVariants

    dpKey = (shapes, currCanvas)
  in
    do
      visited <- get
      if Set.member dpKey visited
        then debugNoLn "% " $ return Nothing
        else
          do
            modify $ Set.insert dpKey
            recFit

fillSmallHolesAround :: Shape -> Canvas -> Int -> Canvas
fillSmallHolesAround aroundThis canvas minSize =
  let
    pointsAround :: [Point]
    pointsAround = distinct $ (neighborsInHole canvas) =<< Set.toList aroundThis

    findSmallHoleStarting :: Point -> Maybe [Point]
    findSmallHoleStarting p =
      let
        bfsExpand :: Point -> State (Set Point) [Point]
        bfsExpand p =
          do
            visited <- get
            if Set.member p visited
              then return []
              else
                do
                  modify $ Set.insert p
                  recRes <- fmap join $ traverse bfsExpand $ neighborsInHole canvas p
                  return (p : recRes)

        bfsRes = evalState (bfsExpand p) Set.empty
      in
        if longerThan minSize bfsRes
          then Nothing
          else Just bfsRes

    holify p s =
      if Set.member p s
        then s
        else Set.union s $ Set.fromList $ fromMaybe [] $ findSmallHoleStarting p

    pointsToFill = foldr holify Set.empty pointsAround
  in
    paint canvas pointsToFill

longerThan :: Int -> [a] -> Bool
longerThan n xs = (length $ take (n+1) xs) > n

neighborsInHole :: Canvas -> Point -> [Point]
neighborsInHole canvas p = filter ((flip Set.notMember) (canvasDrawn canvas)) $ filter (pointWithin (canvasArea canvas)) $ directNeighbors p

trulyNew :: History -> Shape -> Shape -> Bool
trulyNew hist baseShape modifiedShape =
  case Map.lookup baseShape hist of
    Just prevRecord -> modifiedShape > prevRecord
    Nothing -> True

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

debugNoLn :: String -> a -> a
debugNoLn s x = unsafePerformIO (putStr s >> return x)

removeUnusableHoles :: [Shape] -> Canvas -> Canvas
removeUnusableHoles [] canvas = canvas
removeUnusableHoles shapes canvas =
  let
    emptySpaces = findClusters $ negative canvas
    smallestShape = minimum $ map length shapes
    validClusters = filter (\c -> length c >= smallestShape) emptySpaces
    usableFreePoints = Set.unions validClusters
    area = canvasArea canvas
  in
    (area, negative $ (area, usableFreePoints))

canStillFit :: [Shape] -> Canvas -> Bool
canStillFit [] _ = True
canStillFit shapes canvas =
  let
    totalEmptySpace = length $ canvasFreePoints canvas
    totalShapeSize = sum $ map length shapes
  in
    totalEmptySpace >= totalShapeSize

uniqueRotations :: Shape -> [Shape]
uniqueRotations = distinct . allRotations

distinct :: (Eq a) => [a] -> [a]
distinct = nub

canvasFreePoints :: Canvas -> [Point]
canvasFreePoints (area, alreadyDrawn) = filter (\p -> Set.notMember p alreadyDrawn) $ enumArea area

negative :: Canvas -> Shape
negative = Set.fromList . canvasFreePoints

findClusters :: Shape -> [Set Point]
findClusters shape =
  let
    initNext p =
      let
        neighborsInShape = filter (\n -> Set.member n shape) $ directNeighbors p
        candidates = p : neighborsInShape
      in
        minimum candidates

    localGraph = Map.fromList $ map (\x -> (x, initNext x)) $ Set.toList shape

    trackGroup asg p =
      if Map.member p asg
        -- Already known, nothing to do
        then asg
        else
          let
            q = localGraph ! p
            newAsg = trackGroup asg q
          in
            if p == q
              then Map.insert p p asg
              else Map.insert p (newAsg ! q) newAsg

    buildMap = foldl' (trackGroup) Map.empty $ Set.toList shape

    postProcess finalAsg = map (Set.fromList . map fst) $ groupOn snd $ Map.toList finalAsg
  in
    postProcess $ buildMap

debugMap :: (Show b) => (a -> b) -> a -> a
debugMap f x = unsafePerformIO (print (f x) >> return x)

groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\a b -> (f a) == (f b)) . sortOn f

directNeighbors :: Point -> [Point]
directNeighbors (x, y) =
  [
    (x, y+1),
    (x, y-1),
    (x+1, y),
    (x-1, y)
  ]

allRotations :: Shape -> [Shape]
allRotations initShape = scanr (const rotate) initShape $ take 3 $ repeat ()

rotate :: Shape -> Shape
rotate s =
  let
    maxY = maximum $ map snd $ Set.toList s
  in
    setMap (\(x, y) -> (maxY - y, x)) s

--  x -->
-- y
-- |  # # #    # # #
-- v  # #   -> # # #
--    # #          #

-- (0, 0) -> (2, 0)
-- (1, 0) -> (2, 1)
-- (2, 0) -> (2, 2)
-- (0, 1) -> (1, 0)
-- (1, 1) -> (1, 1)
-- (0, 2) -> (0, 0)
-- (1, 2) -> (0, 1)

--  x -->
-- y
-- |  # # # #    # # #
-- v  # #     -> # # #
--    # #            #
--                   #

-- (0, 0) -> (2, 0)
-- (1, 0) -> (2, 1)
-- (2, 0) -> (2, 2)
-- (3, 0) -> (2, 3)
-- (0, 1) -> (1, 0)
-- (1, 1) -> (1, 1)
-- (0, 2) -> (0, 0)
-- (1, 2) -> (0, 1)

translate :: Shape -> Point -> Shape
translate shape tr = setMap (translatePoint tr) shape

setMap :: (Ord b) => (a -> b) -> Set a -> Set b
setMap f = Set.fromList . map f . Set.toList

translatePoint :: Point -> Point -> Point
translatePoint (x, y) (a, b) = (a+x, b+y)

canvasArea :: Canvas -> Area
canvasArea = fst

canvasDrawn :: Canvas -> Shape
canvasDrawn = snd

enumArea :: Area -> [Point]
enumArea (x, y) =
  do
    a <- integerRange 0 (x-1)
    b <- integerRange 0 (y-1)
    return (a, b)

integerRange :: Int -> Int -> [Int]
integerRange a b = [a..b]

place :: Canvas -> Shape -> Maybe Canvas
place canvas@(area, alreadySet) shape =
  if not $ within area shape
    then Nothing
    else if overlaps alreadySet shape
      then Nothing
      else Just $ paint canvas shape

within :: Area -> Shape -> Bool
within area shape = all (pointWithin area) shape

pointWithin :: Area -> Point -> Bool
pointWithin (ax, ay) (px, py) = (px >= 0) && (px < ax) && (py >= 0) && (py < ay)

overlaps :: Set Point -> Set Point -> Bool
overlaps a b = not $ Set.disjoint a b

paint :: Canvas -> Set Point -> Canvas
paint (area, drawn) shape = (area, Set.union drawn shape)
