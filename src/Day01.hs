
whichPart = 2
example = False

main =
  let
    exLines = ["L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82"]
  in
    do
      res <-
        case (example, whichPart) of
          (True, 1) -> return $ part1 exLines
          (False, 1) -> fmap (part1 . lines) (readFile "./data/01.input.txt")
          (True, 2) -> return $ part2 exLines
          (False, 2) -> fmap (part2 . lines) (readFile "./data/01.input.txt")
      print $ res

type Rotation = Either Int Int

parse :: String -> Rotation
parse ('L':xs) = Left (read xs)
parse ('R':xs) = Right (read xs)

rotate :: Rotation -> Int -> Int
rotate (Left x) n = inDial $ n - x
rotate (Right x) n = inDial $ n + x

clockSize = 100

inDial :: Int -> Int
inDial n =
  let
    m = n `mod` clockSize
  in
    if m < 0 then m + clockSize else m

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

rotateFiner :: Rotation -> Int -> [Int]
rotateFiner (Left 0) n = [n]
rotateFiner (Right 0) n = [n]
rotateFiner (Left x) n = n : (rotateFiner (Left $ x - 1) (inDial $ n - 1))
rotateFiner (Right x) n = n : (rotateFiner (Right $ x - 1) (inDial $ n + 1))

rotAllFiner :: [Rotation] -> Int -> [Int]
rotAllFiner (r:rs) n =
  let
    dupSubseq = rotateFiner r n
    (newStart:subseq) = reverse dupSubseq
  in
    subseq ++ (rotAllFiner rs newStart)
rotAllFiner [] n = [n]

part2 :: [String] -> Int
part2 lines =
  let
    parsed = map parse lines
    allPoints = rotAllFiner parsed 50
    finCount = length $ filter ((==) 0) allPoints
  in
    finCount