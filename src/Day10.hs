import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (State, evalState, get, modify, put, runState)
import Data.List (find)
import Data.Maybe (fromJust, catMaybes)
import System.IO.Unsafe (unsafePerformIO)

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

--part2 = sum . map (length . configureJolts)
part2 = sum . map configureJolts . take 10
-- 1 -> 0.56s
-- 10 ->

--configureJolts :: Machine -> [Button]
configureJolts :: Machine -> Int
configureJolts (_, buttons, joltList) =
  let
    targetJolts = Map.fromList $ zipWithIndex joltList
    initJolts = fmap (const 0) targetJolts
    {-initSeq = (initJolts, [])-}
    {-bfsStart = bfs2 buttons targetJolts
    genLevels = evalState bfsStart ([initSeq], Set.empty)-}
    {-astar = sequence $ repeat $ astarOnce buttons targetJolts
    searchResult = evalState astar (Map.singleton 0.0 [initSeq], Set.empty)-}
  in
    --snd $ fromJust $ find (\s -> targetJolts == joltsFrom s) searchResult
    fromJust $ evalState (dp buttons targetJolts) (Map.singleton initJolts (Just 0))

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