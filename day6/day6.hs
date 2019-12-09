import           Data.List (head, tail, groupBy, sort, find, reverse)
import           Data.List.Split       (splitOn)
import           System.Environment    (getArgs)
import Data.Map.Strict (Map, empty, insert, (!), findWithDefault)
import Control.Monad (join)
import Data.Maybe ( fromJust)

data Node =
  Node String
       Int
       [Node]
  deriving (Eq, Ord, Show)

data Pair =
  Pair String
       String
  deriving (Eq, Ord, Show)

commonPrefix :: (Eq a) => Int -> [a] -> [a] -> Int
commonPrefix n [] _ = n
commonPrefix n _ [] = n
commonPrefix n (h1:t1) (h2:t2) = if (h1 /= h2) then n else commonPrefix (n + 1) t1 t2

mkPair :: String -> [String]
mkPair str = splitOn ")" str
createNode :: (Map String [String]) -> Int -> String -> Node
createNode lookupTable d str = 
  let children = findWithDefault [] str lookupTable in
    Node str d (map (createNode lookupTable (d +1)) children)

count :: Node -> Int
count (Node _ d children) = d + (sum $ map count children)

-- parseTree :: [String] -> Node
parseTree strs =
  let pairs = map mkPair strs
      grouped = groupBy (\(a : _) -> \(b : _) -> a == b) $ sort pairs
      lookupTable = foldr (\l -> insert (head $ head l) (map (\(_:x:_) -> x) l)) empty grouped
      node = createNode lookupTable 0 "COM"
  in node

path :: [String] -> String -> Node -> Maybe [String]
path base str (Node n _ children) = if (str == n) then Just base else join $ find (\p -> p /= Nothing) $ map (path (n : base) str) children 


processFile inputFile = do
  input <- readFile inputFile
  let root = parseTree (lines input) 
  let pathYOU = reverse $ fromJust $ path [] "YOU" root
  let pathSAN = reverse $ fromJust $ path [] "SAN" root
  putStrLn (show $ (length pathYOU) + (length pathSAN) - (2 * commonPrefix 0 pathYOU pathSAN))
  putStrLn (show $ reverse $ fromJust $ path [] "YOU" root)
  putStrLn (show $ reverse $ fromJust $ path [] "SAN" root)
    -- putStrLn (show $ count $ root)
    -- putStrLn (unlines $ map show $ parseTree (lines input))

test = do
  processFile "input.txt"

main = do
  args <- getArgs
  case args of
    [input] -> processFile input
    _       -> error "error: specify input file"
