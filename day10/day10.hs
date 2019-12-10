import           Data.List          (concatMap, find, sort, sortOn)
import           System.Environment (getArgs)

data Pos =
  Pos Int
      Int
  deriving (Eq, Ord, Show)

clashes :: Pos -> Pos -> Pos -> Bool
clashes (Pos x0 y0) (Pos x1 y1) (Pos x2 y2) =
  let dx1 = x1 - x0
      dx2 = x2 - x0
      dy1 = y1 - y0
      dy2 = y2 - y0
  in (signum dx1 == signum dx2) &&
     (signum dy1 == signum dy2) && (dy2 * dx1 == dy1 * dx2)

getIndexes :: String -> [Pos]
getIndexes str = concatMap getLineIndex $ zip [0 ..] $ lines str
  where
    getLineIndex :: (Int, String) -> [Pos]
    getLineIndex (y, str) =
      concatMap
        (\(x, c) ->
           if c == '#'
             then [Pos x y]
             else []) $
      zip [0 ..] str

visibleAsteroids :: Pos -> [Pos] -> [Pos] -> [Pos]
visibleAsteroids base [] acc = acc
visibleAsteroids base (h:t) acc =
  if (h /= base) && ((find (clashes base h) acc) == Nothing)
    then visibleAsteroids base t (h : acc)
    else visibleAsteroids base t acc

maxVisibleAsteroids :: String -> [(Pos, Int)]
maxVisibleAsteroids str =
  let positions = getIndexes str
      visibles =
        map (\(p, l) -> (p, length l)) $
        map (\p -> (p, visibleAsteroids p positions [])) positions
  in sortOn (\(_, l) -> l) visibles

sortAsteroids :: Pos -> [[Pos]] -> [Pos] -> [[Pos]]
sortAsteroids base acc [] = acc
sortAsteroids base acc (h:t) =
  let newAcc = addAsteroids base h acc
  in sortAsteroids base newAcc t

addAsteroids :: Pos -> Pos -> [[Pos]] -> [[Pos]]
addAsteroids base pos [] = [[pos]]
addAsteroids base pos acc@(h:t) =
  if (base == pos)
    then acc
    else if (clashes base pos (head h))
           then (pos : h) : t
           else h : (addAsteroids base pos t)

find200 :: Pos -> [[Pos]] -> [Pos]
find200 (Pos x0 y0) positions =
  let circular =
        sortOn
          (\((Pos x y):t) ->
             (pi - (atan2 (fromIntegral (x - x0)) (fromIntegral (y - y0)))))
          positions
      distance =
        map
          (sortOn (\(Pos x y) -> (x - x0) * (x - x0) * (y - y0) * (y - y0)))
          circular
      list = reverse $ takeHeads distance [] []
  in drop 199 list

takeHeads :: [[a]] -> [[a]] -> [a] -> [a]
takeHeads [] [] acc               = acc
takeHeads front [] acc            = takeHeads [] front acc
takeHeads front ((h:[]):back) acc = takeHeads front back (h : acc)
takeHeads front ((h:t):back) acc  = takeHeads (front ++ [t]) back (h : acc)

processFile inputFile = do
  input <- readFile inputFile
  putStrLn (show $ length (getIndexes input))
  putStrLn
    (show
       (find200 (Pos 22 25) (sortAsteroids (Pos 22 25) [] (getIndexes input))))

test = do
  processFile "input.txt"

main = do
  args <- getArgs
  case args of
    [input] -> processFile input
    _       -> putStrLn "error: specify input file"
