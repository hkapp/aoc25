process = part1
example = False

main =
  do
    textInput <-
      if example then
        readFile "./data/09.example.txt"
      else
        readFile "./data/09.input.txt"
    let res = process $ parse textInput
    print $ res

type Point = (Int, Int)

parse :: String -> [Point]
parse = map read . map (\s -> "(" ++ s ++ ")") . lines

part1 = maximum . map surface . combinations

combinations :: [a] -> [(a, a)]
combinations (x:xs) = (map ((,) x) xs) ++ (combinations xs)
combinations [] = []

type Rectangle = (Point, Point)

{-
  x-->
 y
 |
 v
-}
surface :: Rectangle -> Int
surface ((x1, y1), (x2, y2)) =
  let
    width = abs (x2 - x1) + 1
    height = abs (y2 - y1) + 1
  in
    width * height