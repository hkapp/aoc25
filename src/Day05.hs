import Data.Bifunctor (Bifunctor, bimap)

whichPart = 1
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/05.example.txt"
      else
        readFile "./data/05.input.txt"
    let process = case whichPart of
                    1 -> part1
                    --2 -> part2
    let res = process $ parse textInput
    print $ res

type Range = (Id, Id)
type Id = Int

parse :: String -> ([Range], [Id])
parse input =
  case splitOnFirst ((==) "") $ lines input of
    (ranges, ids) ->
      (map parseRange ranges, map parseId ids)

parseId :: String -> Id
parseId = read

parseRange :: String -> Range
parseRange = mapBoth parseId . splitOnFirst ((==) '-')

mapBoth :: (Bifunctor p) => (a -> b) -> p a a -> p b b
mapBoth f = bimap f f

splitOnFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitOnFirst pred input =
  let
    before = takeUntil pred input
    after = tail $ dropUntil pred input
  in
    (before, after)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil pred = takeWhile (not . pred)

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil pred = dropWhile (not . pred)

part1 :: ([Range], [Id]) -> Int
part1 (ranges, ids) = length $ filter id $ map (anyRangeContains ranges) ids

rangeContains :: Range -> Id -> Bool
rangeContains (l, r) i = (i >= l) && (i <= r)

anyRangeContains :: [Range] -> Id -> Bool
anyRangeContains ranges id = any ((flip rangeContains) id) ranges