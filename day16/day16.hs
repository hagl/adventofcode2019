import           Data.Char (digitToInt)

inputString =
  "59731816011884092945351508129673371014862103878684944826017645844741545300230138932831133873839512146713127268759974246245502075014905070039532876129205215417851534077861438833829150700128859789264910166202535524896960863759734991379392200570075995540154404564759515739872348617947354357737896622983395480822393561314056840468397927687908512181180566958267371679145705350771757054349846320639601111983284494477902984330803048219450650034662420834263425046219982608792077128250835515865313986075722145069152768623913680721193045475863879571787112159970381407518157406924221437152946039000886837781446203456224983154446561285113664381711600293030463013"

ex = [1 .. 8]

initialPattern = [0, 1, 0, -1]

repeatList :: [a] -> [a]
repeatList x = x ++ repeatList x

pattern :: Int -> [Int]
pattern n =
  let repeated = initialPattern >>= replicate n
  in tail $ repeatList repeated

input = map digitToInt inputString

--"abc" >>= replicate 3
createOutput :: [Int] -> Int -> Int
createOutput input n =
  let p = pattern n
      z = sum $ map (\(x, y) -> x * y) $ zip input p
  in mod (abs z) 10

step :: [Int] -> [Int]
step inp = map (createOutput inp) [1 .. (length inp)]

run :: [Int] -> Int -> [Int]
run inp count = foldr (\_ -> step) inp [1 .. count]

main = do
  putStrLn $ show $ take 8 $ run input 100
  putStrln $ run2 input

{-
  1 1 1 1 a
  0 1 1 1 b
  0 0 1 1 c
  0 0 0 1 d

d' = d
c' = c + d = c + d'
b' = b + c + d = b + c'
a' = a + b + c + d = a + b'
-}
x = [6, 1, 5, 8]

step2 :: [Int] -> [Int]
step2 list = tail $ map (\y -> mod y 10) $ scanl (+) 0 list

run2 :: [Int] -> String
run2 list =
  let start = foldl (\a -> \b -> 10 * a + b) 0 $ take 7 list
      z = reverse $ drop start $ take ((length input) * 10000) $ repeatList list
      r = foldr (\x -> step2) z [1 .. 100]
  in foldr (++) "" $ map show $ take 8 $ reverse r
