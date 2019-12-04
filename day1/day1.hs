import           System.Environment (getArgs)

fuelReq :: Int -> Int
fuelReq mass = (floor (fromIntegral mass / 3.0) - 2)

fuelReq2 :: Int -> Int
fuelReq2 mass = fuelRec mass 0

fuelRec :: Int -> Int -> Int
fuelRec weight acc =
  let fuel = fuelReq weight
  in if (fuel <= 0)
       then acc
       else fuelRec fuel (acc + fuel)

fuelRequirement :: String -> Int
fuelRequirement = sum . (map fuelReq) . (map read) . lines

fuelRequirementRec :: String -> Int
fuelRequirementRec = sum . (map fuelReq2) . (map read) . lines

processFile inputFile = do
  input <- readFile inputFile
  putStrLn (show (fuelRequirement input))
  putStrLn (show (fuelRequirementRec input))

test = do
  processFile "input.txt"

main = do
  args <- getArgs
  case args of
    [input] -> processFile input
    _       -> putStrLn "error: specify input file"
