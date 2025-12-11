import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (State, evalState, get, modify, put, runState)
import Data.List (find, sortOn, partition, groupBy)
import Data.Maybe (fromJust, catMaybes, isJust, mapMaybe)
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

--part2 = sum . map configureJolts
part2 = map configureJolts . take 24

configureJolts (_, buttons, joltList) =
  let
    targetJolts = Map.fromList $ zipWithIndex joltList
    {-initJolts = fmap (const 0) targetJolts
    sortedJolts = sortOn snd $ zipWithIndex joltList
    sortedJoltIds = map fst sortedJolts
    dpRun = configureAllJolts buttons targetJolts
    dpRes = evalState dpRun Map.empty-}
    sortedButtons = sortOn (Down . length) buttons
    dfs = sequence $ repeat dfsExpand
  in
    --snd $ fromJust $ find (done . fst) $ evalState dfs [(targetJolts, 0, sortedButtons)]
    minimum $ map (sum . Map.elems) $ solve $ toEq (undefined, buttons, joltList)

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
  in
    if not $ feasible eq
      then Nothing
      else
        case deducedBinding of
          Just bnd -> deduce =<< applyBind eq bnd
          Nothing -> Just eq

feasible :: Equation -> Bool
feasible (eqRows, _) = all isNonNeg $ map snd eqRows

isNeg :: Int -> Bool
isNeg n = n < 0

isNonNeg :: Int -> Bool
isNonNeg = not . isNeg

deduceRow :: EqRow -> Maybe (Variable, Value)
deduceRow (x:[], y) = Just (x, y)
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
pickVar (eqRows, _) = head $ fst $ head eqRows

extendVar :: Variable -> Equation -> [Equation]
extendVar var eq =
  let
    eqRows = fst eq
    maxVal = maximum $ map snd eqRows
  in
    mapMaybe deduce $ mapMaybe (\v -> applyBind eq (var, v)) $ integerRange 0 maxVal

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