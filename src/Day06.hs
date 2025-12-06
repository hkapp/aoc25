import Data.List (transpose)
import Data.Bifunctor (second)

process = part1
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/06.example.txt"
      else
        readFile "./data/06.input.txt"

    let res = process $ parse textInput
    print $ res

type Column = [String]

parse :: String -> [Column]
parse = transpose . map words . lines

part1 = sum . map solveCol

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