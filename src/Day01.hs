
whichPart = 1
example = False

main =
  let
    exLines = ["L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82"]
  in
    do
      res <-
        case (example, whichPart) of
          (True, 1) -> return $ part1 exLines
          (False, 1) -> fmap (part1 . lines) (readFile "../data/01.input.txt")
      print $ res

type Rotation = Either Int Int

parse :: String -> Rotation
parse ('L':xs) = Left (read xs)
parse ('R':xs) = Right (read xs)

rotate :: Rotation -> Int -> Int
rotate (Left x) n = inDial $ n - x
rotate (Right x) n = inDial $ n + x

inDial :: Int -> Int
inDial n =
  let
    m = n `mod` 100
  in
    if m < 0 then m + 100 else m

rotAll :: [Rotation] -> Int -> [Int]
rotAll (r:rs) n = n : (rotAll rs $ rotate r n)
rotAll [] n = [n]

part1 :: [String] -> Int
part1 lines =
  let
    parsed = map parse lines
    allPoints = rotAll parsed 50
    finCount = length $ filter ((==) 0) allPoints
  in
    finCount