import           Data.List
import           Data.List.Split       (splitOn)
import           Data.Vector.Primitive (Vector, fromList, toList, (!), (//))
import           System.Environment    (getArgs)

step :: Vector Int -> Int -> Vector Int
step mem pos =
  let op = mem ! pos
  in case op of
       99 -> mem
       _ ->
         let a = mem ! (mem ! (pos + 1))
             b = mem ! (mem ! (pos + 2))
             c = mem ! (pos + 3)
             r =
               if (op == 1)
                 then a + b
                 else a * b
         in step (mem // [(c, r)]) (pos + 4)
        --  in mem // [(c, r)]

run :: String -> Vector Int
run str = step (fromList (map read (splitOn "," str))) 0

run2 :: String -> Int -> Int -> Int
run2 str noun verb =
  let input = fromList (map read (splitOn "," str))
  in (step (input // [(1, noun), (2, verb)]) 0) ! 0

input =
  "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,1,13,23,27,1,6,27,31,2,31,13,35,1,9,35,39,2,39,13,43,1,43,10,47,1,47,13,51,2,13,51,55,1,55,9,59,1,59,5,63,1,6,63,67,1,13,67,71,2,71,10,75,1,6,75,79,1,79,10,83,1,5,83,87,2,10,87,91,1,6,91,95,1,9,95,99,1,99,9,103,2,103,10,107,1,5,107,111,1,9,111,115,2,13,115,119,1,119,10,123,1,123,10,127,2,127,10,131,1,5,131,135,1,10,135,139,1,139,2,143,1,6,143,0,99,2,14,0,0"

pr :: (Int, Int) -> Bool
pr (a, b) = run2 input a b == 19690720

main = do
  putStrLn (show (run2 input 12 2))
  putStrLn (show (find pr [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]))
