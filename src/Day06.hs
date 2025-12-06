import Data.List (transpose)
import Data.Bifunctor (second)

process = part2
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/06.example.txt"
      else
        readFile "./data/06.input.txt"

    let res = process textInput
    print $ res

type Column = [String]

parse1 :: String -> [Column]
parse1 = transpose . map words . lines

part1 = sum . map solveCol . parse1

solveCol :: Column -> Int
solveCol c =
  let
    (strNums, strOp) = splitLast c
    numbers = map read strNums
    op = parseOp strOp
  in
    foldl1 op numbers

splitLast :: [a] -> ([a], a)
splitLast xs = second head $ splitAt ((length xs) - 1) xs

parseOp :: String -> (Int -> Int -> Int)
parseOp "*" = (*)
parseOp "+" = (+)

part2 :: String -> Int
part2 = sum . map solveEq . parse2

solveEq :: Equation -> Int
solveEq (nums, op) = foldl1 op nums

type Equation = ([Int], (Int -> Int -> Int))

parse2 :: String -> [Equation]
parse2 input =
  let
    rows = lines input
    (dataLines, opLine) = splitLast rows
    ops = map parseOp $ words opLine
    dataColumns = transpose dataLines
    groupedColumns = splitWhere null $ map rmSpaces dataColumns
  in
    zip (map (map read) groupedColumns) ops

rmSpaces :: String -> String
rmSpaces = filter ((/=) ' ')

splitWhere     :: (a -> Bool) -> [a] -> [[a]]
splitWhere p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitWhere p s''
                            where (w, s'') = break p s'
