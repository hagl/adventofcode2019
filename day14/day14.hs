import           Control.Monad                 (join)
import           Data.Char                     (digitToInt)
import           Data.List                     (find, groupBy, head, reverse,
                                                sort, tail)
import           Data.List.Split               (splitOn)
import           Data.Map.Strict               (Map, empty, findWithDefault,
                                                insert, (!))
import           Data.Maybe                    (fromJust)
import           Debug.Trace                   (trace)
import           System.Environment            (getArgs)
import           System.Environment            (getArgs)
import           Text.ParserCombinators.Parsec

-- 3 MSKT, 3 NDJG => 5 QDHJT
p_rules :: CharParser () [((String, Int), [(String, Int)])]
p_rules = do
  result <- many p_rule
  eof
  return result

p_rule :: CharParser () ((String, Int), [(String, Int)])
p_rule = do
  requirements <- p_requirements
  string " => "
  result <- p_quantity_material
  char '\n'
  return (result, requirements)

p_quantity_material :: CharParser () (String, Int)
p_quantity_material = do
  q <- integer
  char ' '
  m <- many (oneOf ['A' .. 'Z'])
  return (m, q)

p_requirements :: CharParser () [(String, Int)]
p_requirements = do
  first <- p_quantity_material
  next <- p_quantity_materials
  return (first : next)

p_quantity_materials :: CharParser () [(String, Int)]
p_quantity_materials = (string ", " >> p_requirements) <|> (return [])

integer = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base * x + (digitToInt d)) 0 digits
  seq n (return n)
  where
    base = 10
    baseDigit = digit

insertRule ::
     (Map String (Int, [(String, Int)]))
  -> ((String, Int), [(String, Int)])
  -> (Map String (Int, [(String, Int)]))
insertRule map ((m, q), reqs) = insert m (q, reqs) map

readRules ::
     [((String, Int), [(String, Int)])] -> (Map String (Int, [(String, Int)]))
readRules parsed = foldl insertRule empty parsed

calculateRequirements ::
     [(String, Int)]
  -> [(String, Int)]
  -> (Map String (Int, [(String, Int)]))
  -> Int
calculateRequirements needed available rules =
  case needed of
    [] -> -1
    [("ORE", q)] -> q
    (("ORE", q):t) -> calculateRequirements (t ++ [("ORE", q)]) available rules
    ((m, q):t) ->
      let (req, newAvailable) = checkAvailability m q available
          (required, extra) = applyRule m req rules
          withExtras = addAvailable (m, extra) newAvailable
          newNeeded = foldr addRequirement t required
      in (calculateRequirements newNeeded withExtras rules)

applyRule ::
     String
  -> Int
  -> (Map String (Int, [(String, Int)]))
  -> ([(String, Int)], Int)
applyRule mat 0 rules = ([], 0)
applyRule mat qty rules =
  let (buildQty, reqs) = findWithDefault (1, []) mat rules
      factor =
        ceiling $ ((fromIntegral qty) / (fromIntegral buildQty) :: Double)
      needed = map (\(s, q) -> (s, q * factor)) reqs
      extra = (buildQty * factor) - qty
  in (needed, extra)

checkAvailability :: String -> Int -> [(String, Int)] -> (Int, [(String, Int)])
checkAvailability mat qty available =
  case find (byMaterial mat) available of
    Nothing -> (qty, available)
    Just (_, qa) ->
      if (qty >= qa)
        then (qty - qa, filter (not . byMaterial mat) available)
        else (0, (mat, qa - qty) : filter (not . byMaterial mat) available)
  where
    byMaterial mat = (\(s, _) -> s == mat)

addAvailable :: (String, Int) -> [(String, Int)] -> [(String, Int)]
addAvailable (mat, 0) list = list
addAvailable (mat, qty) [] = [(mat, qty)]
addAvailable (mat, qty) ((m, q):t) =
  if (mat == m)
    then (m, q + qty) : t
    else (m, q) : (addAvailable (mat, qty) t)

addRequirement = addAvailable

-- findMaxFuel :: Int -> (Map String (Int, [(String, Int)])) -> Int
-- findMaxFuel limit rules =
process parsed = do
  putStrLn (show (calculateRequirements [("FUEL", 1)] [] (readRules parsed)))
  putStrLn
    (show (calculateRequirements [("FUEL", 1993284)] [] (readRules parsed)))
  putStrLn
    (show (calculateRequirements [("FUEL", 1993285)] [] (readRules parsed)))

processFile inputFile = do
  input <- readFile inputFile
  case parse p_rules inputFile input of
    Right (parsed) -> process (parsed)
    Left (error)   -> putStrLn (show error)

test = do
  processFile "input.txt"

main = do
  args <- getArgs
  case args of
    [input] -> processFile input
    _       -> putStrLn "error: specify input file"
