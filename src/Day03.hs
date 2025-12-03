import Data.List (sort, find, sortOn)
import Data.Tuple (swap)
import Data.Maybe (fromJust, isNothing)
import Data.Ord (Down(Down), getDown)
import qualified Data.Bifunctor as Bifunctor
import System.IO.Unsafe (unsafePerformIO)

whichPart = 2
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/03.example.txt"
      else
        readFile "./data/03.input.txt"
    let process = case whichPart of
                    1 -> part1
                    2 -> part2
    let res = process $ parse textInput
    print $ res

type Digit = Char
type Bank a = [a]

parse :: String -> [Bank Char]
parse = lines

part1 :: [Bank Digit] -> Int
part1 = sum . map (digitJoltage . maxJoltage)

maxJoltage :: (Ord a) => Bank a -> (a, a)
maxJoltage bank =
  let
    (before, maxDigit, after) = splitFirstMax bank
    digitPair =
      case after of
        [] ->
          -- No digit after max, need to retrieve from before
          (maximum before, maxDigit)
        _ ->
          -- Retrieve the digit after that
          (maxDigit, maximum after)
  in
    digitPair

splitFirstMax :: (Ord a) => [a] -> ([a], a, [a])
splitFirstMax xs =
  let
    m = maximum xs
    before = takeWhile ((/=) m) xs
    after = drop ((length before) + 1) xs
  in
    (before, m, after)

digitJoltage :: (Digit, Digit) -> Int
digitJoltage = read . pairToList

pairToList :: (a, a) -> [a]
pairToList (a, b) = [a, b]

part2 = sum . map (finalize . pick 12 . prepare)

prepare :: (Ord a) => Bank a -> [(a, Int)]
prepare bank =
  let
    -- 1. sort by value then by index
    sorted = sortOn Down renumbered
    -- 2. renumber the indices to say how many are left after that
    totLen = length bank
    renumbered = map (Bifunctor.second (\pos -> totLen - pos - 1)) $ map swap $ zipWithIndex bank
  in
    sorted

pick :: (Show a) => Int -> [(a, Int)] -> [(a, Int)]
pick 0 _ = []
pick n xs =
  let
    goodEnough (_, remAfter) = remAfter >= n - 1
    currPick = fromJust $ debug isNothing (n, xs) $ find goodEnough xs
    willRemAfter = snd currPick
    rem = filter (\(_, remAfter) -> remAfter < willRemAfter) xs
  in
    currPick : (pick (n-1) rem)

debug :: (Show b) => (a -> Bool) -> b -> a -> a
debug predicate toPrint value =
  if predicate value then
    unsafePerformIO $ do
                        print toPrint
                        return value
  else
    value

finalize :: [(Digit, Int)] -> Int
finalize = read . map fst . sortOn (Down . snd)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex =
  let
    zwi _ [] = []
    zwi n (x:xs) = (n, x) : (zwi (n+1) xs)
  in
    zwi 0