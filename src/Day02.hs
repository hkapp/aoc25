
whichPart = 1
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/02.example.txt"
      else
        readFile "./data/02.input.txt"
    let res = part1 $ parse textInput
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