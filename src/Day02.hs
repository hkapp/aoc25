
whichPart = 2
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/02.example.txt"
      else
        readFile "./data/02.input.txt"
    let process = case whichPart of
                    1 -> part1
                    2 -> part2
    let res = process $ parse textInput
    print $ res

type Range = (Int, Int)

parse :: String -> [Range]
parse = map parseRange . splitStr ','

-- Source - https://stackoverflow.com/a
-- Posted by Steve
-- Retrieved 2025-12-02, License - CC BY-SA 2.5
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitStr c = wordsWhen ((==) c)

parseRange :: String -> Range
parseRange = buildRange . map read . splitStr '-'

buildRange (l:r:[]) = (l, r)

part1 :: [Range] -> Int
part1 ranges =
  let
    allValues = ranges >>= enumRange
    invalidIds = filter isInvalid allValues
  in
    sum invalidIds

enumRange :: Range -> [Int]
enumRange (n, m) =
  if n == m then
    [n]
  else
    n:(enumRange ((n+1), m))

isInvalid :: Int -> Bool
isInvalid = symmetrical . show

symmetrical :: String -> Bool
symmetrical s =
  if even $ length s then
    case splitAt ((length s) `div` 2) s of
      (l, r) -> l == r
  else
    False

part2 :: [Range] -> Int
part2 ranges =
  let
    allValues = ranges >>= enumRange
    invalidIds = filter isInvalid2 allValues
  in
    sum invalidIds

isInvalid2 :: Int -> Bool
isInvalid2 x =
  let
    s = show x

    isRep n =
      let
        chunks = splitChunks n s
      in
        all ((==) (head chunks)) chunks

    validLens = take ((length s) - 1) (integers 1)
  in
    any isRep validLens

integers :: Int -> [Int]
integers start = start : (integers (start + 1))

splitChunks :: Int -> [a] -> [[a]]
splitChunks n [] = []
splitChunks n xs =
  let
    (chunk, rem) = splitAt n xs
  in
    chunk : (splitChunks n rem)