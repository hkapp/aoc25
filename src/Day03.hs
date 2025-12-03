
whichPart = 1
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

part2 = undefined