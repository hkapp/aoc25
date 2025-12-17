import Data.Bifunctor (second, first)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (State, evalState, get, modify, put, runState)
import Data.List (find, sortOn, partition, groupBy, sort, group)
import Data.Maybe (fromJust, catMaybes, isJust, mapMaybe, fromMaybe, isNothing)
import System.IO.Unsafe (unsafePerformIO)
import Data.Ord (Down(Down))
import Control.Exception (assert)
import Data.Tuple (swap)

process = part2
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/10.example.txt"
      else
        readFile "./data/10.input.txt"
    let res = process $ parse textInput
    print $ res

parse :: String -> [Machine]
parse = map parseMachine . lines

type Machine = (Lights, [Button], [Int])

parseMachine :: String -> Machine
parseMachine input =
  let
    (lights, rem) = splitOnFirst ((==) ' ') input
    (buttonsWS, partialJoltage) = splitOnFirst ((==) '{') rem
    buttons = dropLast 1 buttonsWS
    joltage = '{' : partialJoltage
  in
    (parseLights lights, parseButtons buttons, parseJoltage joltage)

type LightId = Int
type LightState = Bool
type Lights = Map LightId LightState

parseLights :: String -> Lights
parseLights = Map.fromList . zipWithIndex . map parseLightState . dropLast 1 . drop 1

parseLightState :: Char -> Bool
parseLightState '.' = False
parseLightState '#' = True

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip (integers 0)

integers :: Int -> [Int]
integers n = n : (integers (n+1))

type Button = [LightId]

parseButtons :: String -> [Button]
parseButtons = read . surround '[' ']' . replace ' ' ',' . replace ')' ']' . replace '(' '['

surround :: a -> a -> [a] -> [a]
surround before after xs = before : (xs ++ [after])

parseJoltage :: String -> [Int]
parseJoltage = read . replace '}' ']' . replace '{' '['

replace :: (Eq a, Functor f) => a -> a -> f a -> f a
replace old new = fmap (\x -> if x == old then new else x)

splitOnFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitOnFirst p = second tail . span (not . p)

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

part1 :: [Machine] -> Int
part1 = sum . map (length . configure)

pushButton :: Button -> Lights -> Lights
pushButton bConns initLights = foldr toggle initLights bConns

toggle :: LightId -> Lights -> Lights
toggle = Map.adjust not

configure :: Machine -> [Button]
configure (targetLights, buttons, _) =
  let
    initLights = fmap (const False) targetLights
    initSeq = (initLights, [])
    bfsStart = bfs buttons
    genLevels = evalState bfsStart ([initSeq], Set.empty)
    searchResult = concat $ genLevels
  in
    snd $ fromJust $ find (\s -> targetLights == lightsFrom s) searchResult

type PushSeq = (Lights, [Button])

bfsExpand :: [Button] -> PushSeq -> State (Set Lights) [PushSeq]
bfsExpand machineButtons currPath =
  do
    modify $ Set.insert (lightsFrom currPath)
    visited <- get
    let nextSeqs = map (extendSeq currPath) machineButtons
    let unvisitedNext = filter (\s -> Set.notMember (lightsFrom s) visited) nextSeqs
    return unvisitedNext

lightsFrom :: PushSeq -> Lights
lightsFrom = fst

extendSeq :: PushSeq -> Button -> PushSeq
extendSeq (currLights, buttonPresses) pushedButton =
  (pushButton pushedButton currLights, pushedButton:buttonPresses)

bfsLevel :: [Button] -> State ([PushSeq], Set Lights) [PushSeq]
bfsLevel availableButtons =
  do
    (currLevelStart, visited) <- get
    let willExpand = fmap concat $ traverse (bfsExpand availableButtons) currLevelStart
    let (nextLevel, newVisited) = runState willExpand visited
    put (nextLevel, newVisited)
    return nextLevel

bfs :: [Button] -> State ([PushSeq], Set Lights) [[PushSeq]]
bfs availableButtons = sequence $ repeat (bfsLevel availableButtons)

part2 = sum . map configureJolts

configureJolts (_, buttons, joltList) =
  let
    startingEq = toEq (undefined, buttons, joltList)
  in
    score $ fromJust $ evalState (solveBetter startingEq) Nothing

integerRange :: Int -> Int -> [Int]
integerRange a b = take (b - a + 1) $ integers a

multiMapInsert :: (Ord k) => k -> a -> Map k [a] -> Map k [a]
multiMapInsert key newValue currMap =
  let
    appendIfExists (Just xs) = Just (newValue:xs)
    appendIfExists Nothing = Just [newValue]
  in
    Map.alter appendIfExists key currMap

type Equation = ([EqRow], Binds)
type EqRow = ([Variable], Value)
type Binds = Map Variable Value
type Value = Int
type Variable = LightId

deduce :: Equation -> Maybe Equation
deduce eq =
  let
    (eqRows, binds) = eq
    deducedBinding = safeHead $ mapMaybe deduceRow eqRows
    newRows = deduceRowSubsets eqRows
  in
    if not $ feasible eq
      then Nothing
      else
        case deducedBinding of
          Just bnd -> deduce =<< applyBind eq bnd
          Nothing ->
            if newRows /= eqRows
              then deduce (newRows, binds)
              else Just eq

deduceRowSubsets :: [EqRow] -> [EqRow]
deduceRowSubsets currRows =
  let
    setForm = map (first Set.fromList) currRows
    reduceThisOne (bigVars, bigValue) (smallVars, smallValue) = (bigVars `Set.difference` smallVars, bigValue - smallValue)
    reduceOneSet s@(xs, _) = fromMaybe s $ fmap (reduceThisOne s) $ find (\(ys, _) -> ys `Set.isProperSubsetOf` xs) setForm
    reducedSets = map reduceOneSet setForm
  in
    map (first Set.toList) reducedSets

test _ =
  let
    --startingRows = [([2,8,10],32),([8,10,11],26),([9,10],14),([8,10],20),([2,10,11],29),([2,9,10],26),([2,8],23)]
    startingRows = [([6,10],29),([0,4],24),([0,10],23),([0,6],30),([0,10],25)]
    startingEq = (startingRows, Map.empty)
  in
    --(deduce startingEq, deduceRowSubsets startingRows)
    feasible startingEq

feasible :: Equation -> Bool
feasible (eqRows, _) =
  let
    nothingNegative = all isNonNeg $ map snd eqRows

    setForm = map (first Set.fromList) eqRows
    multiMap = foldr (uncurry multiMapInsert) Map.empty setForm
    containsDifferences xs = (length $ Set.fromList xs) > 1
    noUnresolvableDuplicates = isNothing $ find containsDifferences $ Map.elems multiMap
  in
    nothingNegative && noUnresolvableDuplicates

isNeg :: Int -> Bool
isNeg n = n < 0

isNonNeg :: Int -> Bool
isNonNeg = not . isNeg

deduceRow :: EqRow -> Maybe (Variable, Value)
deduceRow (x:[], y) = Just (x, y)
deduceRow (x:xs, 0) = debug4 "0 " $ Just (x, 0)
deduceRow _ = Nothing

applyBind :: Equation -> (Variable, Value) -> Maybe Equation
applyBind (currRows, currBinds) (bndVar, bndValue) =
  let
    newBinds = assert (Map.notMember bndVar currBinds) (Map.insert bndVar bndValue currBinds)
    bindRow (rowVars, rowValue) =
      case partition ((==) bndVar) rowVars of
        ([], unaffected) -> (unaffected, rowValue)
        (x:[], remVars) -> (remVars, rowValue - bndValue)

    rmEmpty ([], 0) = Nothing
    --rmEmpty ([], n) = error $ "Invalid bind apply"
    rmEmpty row = Just row

    newRows = mapMaybe rmEmpty $ map bindRow currRows
  in
    if any (null . fst) newRows
      then Nothing
      else Just (newRows, newBinds)

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing

solve :: Equation -> [Binds]
solve eq =
  let
    (currRows, currBinds) = eq
    var = pickVar eq
    solutions = solve =<< extendVar var eq
  in
    if eqDone eq
      then [currBinds]
      else if not $ feasible eq
        then []
        else solutions

eqDone :: Equation -> Bool
eqDone (eqRows, _) = null eqRows

pickVar :: Equation -> Variable
pickVar (eqRows, _) = head $ fst $ head $ sortOn fanout2 eqRows

fanout2 :: EqRow -> Int
fanout2 (lhs, rhs) = rhs ^ ((length lhs) - 1)

extendVar :: Variable -> Equation -> [Equation]
extendVar var eq =
  let
    eqRows = fst eq
    maxVal = minimum $ map snd $ rowsContaining var eqRows
  in
    mapMaybe deduce $ mapMaybe (\v -> applyBind eq (var, v)) $ reverse $ integerRange 0 maxVal

rowsContaining :: Variable -> [EqRow] -> [EqRow]
rowsContaining var = filter (elem var . fst)

toEq :: Machine -> Equation
toEq (_, buttons, jolts) =
  let
    buttonToJolt = (\(i, bs) -> map ((,) i) bs) =<< zipWithIndex buttons
    joltToButton = map (\xs -> (snd $ head xs, map fst xs)) $ groupOn snd buttonToJolt
    rows = map swap $ zip jolts $ map snd $ sortOn fst joltToButton
  in
    fromJust $ deduce (rows, Map.empty)

groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\a b -> (f a) == (f b)) . sortOn f

solveBetter :: Equation -> State (Maybe Binds) (Maybe Binds)
solveBetter eq =
  let
    (currRows, currBinds) = eq
    var = pickVar eq

    recSolve =
      do
        solutions <- traverse solveBetter $ extendVar var eq
        -- Note: traverse is already taking care of filtering out the best ones
        -- Also note: this is not guaranteed to be the same as the current best in `get`,
        -- as the latter may be coming from a different branch of the search tree
        let bestSolution = safeLast $ mapMaybe id solutions
        return bestSolution

    stateSolve =
      do
        currBest <- get
        if canBeBetter currBest eq
          then recSolve
          else return $ debug4 ("< ") Nothing
  in
    if eqDone eq
      then competitor currBinds
      else if not $ feasible eq
        then return Nothing
        else stateSolve

debug4 :: String -> a -> a
debug4 text value = unsafePerformIO (putStr text >> return value)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

competitor :: Binds -> State (Maybe Binds) (Maybe Binds)
competitor newValue =
  let
    b1 `isBetterThan` b2 = (score b1) < (score b2)

    becomeBest =
      do
        put $ debug3 ("Found new best: " ++ (show newValue)) $ Just newValue
        return $ Just newValue
  in
    do
      currBest <- get
      case currBest of
        Just oldValue ->
          if newValue `isBetterThan` oldValue then
            becomeBest
          else
            -- New is not better, ignore
            return Nothing
        Nothing ->
          -- First solution found
          becomeBest

debug3 :: String -> a -> a
debug3 text value = unsafePerformIO (putStrLn text >> return value)

score :: Binds -> Int
score = sum . Map.elems

canBeBetter :: (Maybe Binds) -> Equation -> Bool
canBeBetter Nothing _ = True
canBeBetter (Just bestBinds) eq = (leastPossibleScore eq) < (score bestBinds)

leastPossibleScore :: Equation -> Int
leastPossibleScore (eqRows, currBinds) =
  let
    setForm = map (first Set.fromList) eqRows

    solveSimpler :: [(Set Variable, Value)] -> Int
    solveSimpler [] = 0
    solveSimpler ((vars, value) : rem) =
      let
        notPicked = solveSimpler rem
        remIfPicked = filter (Set.disjoint vars . fst) rem
        ifPicked = value + solveSimpler remIfPicked
      in
        max notPicked ifPicked
  in
    (score currBinds) + (solveSimpler setForm)
