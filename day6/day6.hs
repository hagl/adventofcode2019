import           Data.List
import           Data.List.Split       (splitOn)
import           Data.Vector.Primitive (Vector, fromList, toList, (!), (//))
import           System.Environment    (getArgs)

data Node =
  Node String
       Int
       [Node]
  deriving (Eq, Ord, Show)

data Pair =
  Pair String
       String
  deriving (Eq, Ord, Show)

mkPair :: String -> Pair
mkPair str =
  let list = splitOn ")" str
  in Pair (head list) (head (tail list))

-- parseTree :: [String] -> Node
parseTree strs =
  let pairs = map mkPair strs
  in (groupBy (\(Pair a _) -> \(Pair b _) -> a == b) $ sort pairs)

processFile inputFile = do
  input <- readFile inputFile
  putStrLn (unlines $ map show $ parseTree (lines input))

test = do
  processFile "input.txt"

main = do
  args <- getArgs
  case args of
    [input] -> processFile input
    _       -> error "error: specify input file"
