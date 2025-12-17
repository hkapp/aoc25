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
--part2 = map configureJolts . take 128
-- 23 -> [40,114,104,38,30,25,34,73,222,148,197,86,59,65,37,146,58,239,53,39,114,88,157,341]

configureJolts (_, buttons, joltList) =
  let
    targetJolts = Map.fromList $ zipWithIndex joltList
    startingEq = toEq (undefined, buttons, joltList)
  in
    score $ fromJust $ evalState (solveBetter startingEq) Nothing
    --leastPossibleScore startingEq
    -- 20 and under: 400ms
    -- 21: 9.8s
    -- 22: 12s
    -- 23: 13s
    -- 24: 24s
    -- 32: 28s
    -- 64: 29s
    -- 128: 1m38s

configureAllJolts :: [Button] -> Jolts -> State (Map Jolts (Maybe Int)) (Maybe Int)

configureAllJolts availableButtons targetJolts =
  if done targetJolts then
    -- We are done, trivial
    return $ Just 0
  else
    do
      cache <- get
      let dpKey = debug2 (availableButtons, targetJolts) targetJolts
      if Map.member dpKey cache then
        -- DP cache hit: return
        return $ fromJust $ Map.lookup dpKey cache
      else
        -- Not yet computed, need to compute it now
        do
          let (partialConfigs, nPresses) = configureEasiestJolt availableButtons targetJolts
          -- Work on the most joltage reduced option first
          -- The first of those that returns a valid config has to be the best
          let sortedPartials = sortOn (sum . Map.elems) partialConfigs
          fullConfigurations <- traverse (configureAllJolts availableButtons) sortedPartials
          let result = fmap ((+) nPresses) =<< find isJust fullConfigurations
          modify $ debug . Map.insert dpKey result
          return result

done :: Jolts -> Bool
done = all ((==) 0) . Map.elems

debug2 :: (Show a) => a -> b -> b
debug2 disp ret = ret--unsafePerformIO $ print disp >> return ret

type Pos = Int

configureEasiestJolt :: [Button] -> Jolts -> ([Jolts], Int)
configureEasiestJolt allButtons targetJolts =
  let
    -- Remove buttons that would ruin already configured jolts
    touchesFinishedJolt b = any (\j -> 0 == (fromJust $ Map.lookup j targetJolts)) b
    pressableButtons = filter (not . touchesFinishedJolt) allButtons

    easiestJolt = findEasiestJolt pressableButtons targetJolts
  in
    configureOneJolt pressableButtons targetJolts easiestJolt

findEasiestJolt :: [Button] -> Jolts -> Int
findEasiestJolt pressableButtons targetJolts =
  let
    unfinishedJolts = filter (\(_, j) -> j /= 0) $ Map.toList targetJolts
    pow = (^)
    fanout (id, jolt) = jolt `pow` (length $ onlyRelevant id pressableButtons)
    easiestJolt = fst $ minimumBy fanout unfinishedJolts
  in
    easiestJolt

onlyRelevant :: Pos -> [Button] -> [Button]
onlyRelevant consideredJolt = filter (elem consideredJolt)

minimumBy :: (Ord b) => (a -> b) -> [a] -> a
minimumBy f = head . sortOn f

maximumBy :: (Ord b) => (a -> b) -> [a] -> a
maximumBy f = head . sortOn (Down . f)

configureOneJolt :: [Button] -> Jolts -> Pos -> ([Jolts], Int)
configureOneJolt availableButtons targetJolts consideredJolt =
  let
    relevantButtons = onlyRelevant consideredJolt availableButtons
    nPresses = fromJust $ Map.lookup consideredJolt targetJolts
    possibleButtonPresses = decompose relevantButtons nPresses
    appliedConfigs = map (foldr (flip pushJoltRev) targetJolts) possibleButtonPresses
    validConfigs = filter (not . overshot2) appliedConfigs
  in
    if null relevantButtons && nPresses > 0
      -- With no available buttons, we cannot configure anything
      then ([], error "Cannot configure with no buttons")
      else (validConfigs, nPresses)

decompose :: [a] -> Int -> [[a]]
decompose _ 0 = [[]]
decompose [] _ = error "Invalid decompose"
decompose (x:[]) n = [take n $ repeat x]
decompose (x:xs) n =
  (integerRange 0 n) >>= (\m -> map ((++) (take m $ repeat x)) $ decompose xs (n-m))

integerRange :: Int -> Int -> [Int]
integerRange a b = take (b - a + 1) $ integers a

type Jolts = Map Int Int
type JoltSeq = (Jolts, [Button])

bfsExpand2 :: [Button] -> Jolts -> JoltSeq -> State (Set Jolts) [JoltSeq]
bfsExpand2 machineButtons targetJolts currPath =
  do
    modify $ Set.insert (joltsFrom currPath)
    visited <- get
    let nextSeqs = map (extendJolt currPath) machineButtons
    let unvisitedNext = filter (\s -> Set.notMember (joltsFrom s) visited) nextSeqs
    let candidates = filter (\s -> not $ overshot targetJolts (joltsFrom s)) unvisitedNext
    return candidates

overshot :: Jolts -> Jolts -> Bool
overshot targetJolts currJolts =
  any id $ Map.elems $ Map.intersectionWith (\tgt j -> j > tgt) targetJolts currJolts

joltsFrom :: JoltSeq -> Jolts
joltsFrom = fst

extendJolt :: JoltSeq -> Button -> JoltSeq
extendJolt (currJolts, buttonPresses) pushedButton =
  (pushJolt pushedButton currJolts, pushedButton:buttonPresses)

pushJolt :: Button -> Jolts -> Jolts
pushJolt bs js = foldr (Map.adjust ((+) 1)) js bs

bfsLevel2 :: [Button] -> Jolts -> State ([JoltSeq], Set Jolts) [JoltSeq]
bfsLevel2 availableButtons targetJolts =
  do
    (currLevelStart, visited) <- get
    let willExpand = fmap concat $ traverse (bfsExpand2 availableButtons targetJolts) currLevelStart
    let (nextLevel, newVisited) = runState willExpand visited
    put (nextLevel, newVisited)
    return nextLevel

bfs2 :: [Button] -> Jolts -> State ([JoltSeq], Set Jolts) [[JoltSeq]]
bfs2 availableButtons targetJolts = sequence $ repeat (bfsLevel2 availableButtons targetJolts)

astarOnce :: [Button] -> Jolts -> State (PQueue, Set Jolts) JoltSeq
astarOnce availableButtons targetJolts =
  do
    (pq, visited) <- get
    let (seq, newPq) = pqPick pq
    put (newPq, visited)
    let currJolts = joltsFrom seq
    if Set.member currJolts visited then
      -- Already visited, skip
      astarOnce availableButtons targetJolts
    else
      do
        let newVisited = Set.insert currJolts visited
        let next = map (extendJolt seq) availableButtons
        let candidates = filter (not . overshot targetJolts . joltsFrom) next
        let newNewPq = foldr pqInsert newPq candidates
        put (newNewPq, newVisited)
        return seq

type PKey = JoltSeq
type PQueue = Map Double [PKey]

pqInsert :: PKey -> PQueue -> PQueue
pqInsert jseq = multiMapInsert (astarCriteria jseq) jseq

astarCriteria :: PKey -> Double
astarCriteria (currJolts, pressedButtons) =
  (fromIntegral $ sum $ Map.elems currJolts) / (fromIntegral $ length pressedButtons)

multiMapInsert :: (Ord k) => k -> a -> Map k [a] -> Map k [a]
multiMapInsert key newValue currMap =
  let
    appendIfExists (Just xs) = Just (newValue:xs)
    appendIfExists Nothing = Just [newValue]
  in
    Map.alter appendIfExists key currMap

pqPick :: PQueue -> (PKey, PQueue)
pqPick pq =
  let
    ((key, vs), subPQ) = Map.deleteFindMax pq
  in
    case vs of
      x:y:ys -> (x, Map.insert key (y:ys) subPQ)
      x:[] -> (x, subPQ)

dp :: [Button] -> Jolts -> State (Map Jolts (Maybe Int)) (Maybe Int)
dp availableButtons targetJolts =
  do
    cache <- get
    case Map.lookup targetJolts cache of
      Just n -> return n
      Nothing ->
        -- Not in the cache, need to compute
        do
          let children = map (pushJoltRev targetJolts) availableButtons
          let validChildren = filter (not . overshot2) children
          childrenPresses <- traverse (dp availableButtons) validChildren
          let feasibleChildrenPresses = catMaybes childrenPresses
          let answer = case feasibleChildrenPresses of
                        [] -> Nothing  -- We cannot reach the end from here
                        _  -> Just $ 1 + minimum feasibleChildrenPresses
          modify $ debug . Map.insert targetJolts answer
          return answer

debug :: Map k a -> Map k a
debug m =
  if (length m) `mod` 10000 == 0
    then unsafePerformIO $ print (length m) >> return m
    else m

pushJoltRev :: Jolts -> Button -> Jolts
pushJoltRev js bs = foldr (Map.adjust (\j -> j - 1)) js bs

overshot2 :: Jolts -> Bool
overshot2 js = any (\j -> j < 0) $ Map.elems js

dfsExpand :: State [(Jolts, Int, [Button])] (Jolts, Int)
dfsExpand =
  do
    stack <- get
    let (picked:remStack) = stack
    let (currJolts, pressCount, availableButtons) = picked

    -- Either we press the next button
    let ifPressedJolts = pushJoltRev currJolts (head availableButtons)
    let ifPressed = if validJolts ifPressedJolts
                      then [(ifPressedJolts, pressCount + 1, availableButtons)]
                      else []

    -- Or we don't
    let ifNotPressed = if length availableButtons > 1
                        then [(currJolts, pressCount, tail availableButtons)]
                        else []

    let newStack = ifPressed ++ ifNotPressed ++ remStack
    put newStack
    return (currJolts, pressCount)

validJolts :: Jolts -> Bool
validJolts = all (\j -> j >= 0) . Map.elems

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
  {-let
    sortedButtons = sortOn Down $ map length $ group $ sort $ fst =<< eqRows

    solveSimpler _ [] = 0
    solveSimpler (b:bs) rhs =
      let
        (picked, rem) = splitAt b $ sortOn Down rhs
        presses = last picked
        newPicked = filter ((/=) 0) $ map (\v -> v - presses) picked
      in
        case rem of
          [] -> maximum picked
          _  -> presses + solveSimpler bs (newPicked ++ rem)
    solveSimpler [] rhs = error $ "Reached unscorable with eq=" ++ (show eqRows) ++ ", buttons=" ++ (show sortedButtons) ++ ", remRhs=" ++ (show rhs)
  in
    (score currBinds) + (solveSimpler sortedButtons (map snd eqRows))-}

-- This is wrong:
-- "least possible" : [40,107,68,44,30,25,34,58,202,139,188,76,50,47,37,146,54,231,53,38,113,83,150]
-- actual: [40,118,104,38,30,25,34,73,222,148,197,86,59,65,37,146,58,239,53,39,114,88,164]
-- -> #4 is "below" the "least possible"
-- Solution for these entries:
-- [fromList [(0,0),(1,18),(2,11),(3,11)],fromList [(0,13),(1,4),(2,22),(3,0),(4,4),(5,13),(6,18),(7,5),(8,11),(9,28)],fromList [(0,13),(1,1),(2,13),(3,7),(4,11),(5,0),(6,17),(7,19),(8,7),(9,1),(10,11),(11,4)],fromList [(0,6),(1,17),(2,15),(3,0)]]
-- #4 = fromList [(0,6),(1,17),(2,15),(3,0)]
-- Input for #4 is:
-- [.##..#] (0,1,2,3,5) (0,2,3,4) (0,1,3,4) (0,1,3,4,5) {38,21,23,38,32,6}
-- With our idea we have:
-- buttons = [5, 5, 4, 4]
-- steps =
--   [38,38,32,23,21,6] -> [17,17,11,2,0,6] (21)
--   [17,17,11,6,2] -> [15, 15, 9, 4, 0] (+2 -> 23)
--   [15, 15, 9, 4] -> [11, 11, 5, 0] (+4 -> 27)
--   [11, 11, 5] -> [6, 6, 0] (+5 -> 32)
--   -> here we are not using the full effiency of that button
--      we only decrease 3 buttons instead of 4
--   -> we could simply reattribute the counter values and get:
--   [9, 9, 9] -> [0, 0, 0] (+9 -> 38)
--     which is the right value
--     but is this just chance?
--     when should we apply this if so?
--     what does this even mean?
--  once we've removed enough variables from the start isn't it enough to then take the max?
-- New least possible scores:
-- [40,107,68,38,30,25,34,58,202,139,188,76,50,47,37,146,54,231,46,38,113,83,150]
-- These are all legit
-- What is the difference with just taking the max?
-- Some of them are really different
-- 24th is 307 least possible
--  {285,105,96,252,301,277,106,272,246,249}
-- Hopefully there is a bigger difference when some buttons have already been pressed

debug5 :: (Show a) => a -> a
debug5 x = unsafePerformIO (print x >> return x)
