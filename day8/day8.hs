import           Data.List          (group, last, length, sort, transpose)
import           Data.List.Split    (chunksOf)
import           System.Environment (getArgs)

-- decodeImage :: String -> Int
decodeImage image =
  let chunks = chunksOf (25 * 6) image
  sorted = map sort chunks
  groups = sort sorted
  lastGroup = map length $ group $ last groups
  in foldl (*) 1 $ tail lastGroup
  
-- decodeImage :: String -> String
decodeImage2 image =
  let chunks = chunksOf (25 * 6) image
      transposed = transpose chunks
      withoutTransparency = map (head . (filter (\c -> c /= '2'))) transposed
      printable =
        map
          (\c ->
             if c == '0'
               then '◽'
               else '◾')
          withoutTransparency
  in unlines $ chunksOf 25 printable

processFile inputFile = do
  input <- readFile inputFile
  putStrLn (show (decodeImage input))
  putStrLn ((decodeImage2 input))

test = do
  processFile "input.txt"

main = do
  args <- getArgs
  case args of
    [input] -> processFile input
    _       -> putStrLn "error: specify input file"
