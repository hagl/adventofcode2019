import           Data.List
import           Data.List.Split       (splitOn)
import           Data.Vector.Primitive (Vector, fromList, toList, (!), (//))
import           Debug.Trace           (trace)
import           System.Environment    (getArgs)

data Instruction
  = Add Bool
        Bool
  | Mult Bool
         Bool
  | Input
  | Output Bool
  | JumpTrue Bool
             Bool
  | JumpFalse Bool
              Bool
  | Less Bool
         Bool
  | Equal Bool
          Bool
  | Stop
  deriving (Eq, Ord, Show)

parseInstruction :: Int -> Instruction
parseInstruction number =
  let op = number `mod` 100
      modeC = (number `div` 100) `mod` 10 == 1
      modeB = (number `div` 1000) `mod` 10 == 1
      modeA = (number `div` 10000) `mod` 10 == 1
  in case mod number 100 of
       01 -> Add modeC modeB
       02 -> Mult modeC modeB
       03 -> Input
       04 -> Output modeC
       05 -> JumpTrue modeC modeB
       06 -> JumpFalse modeC modeB
       07 -> Less modeC modeB
       08 -> Equal modeC modeB
       99 -> Stop
       _  -> error "Error"

newtype State =
  State (Vector Int, [Int], [Int])
  deriving (Eq, Ord, Show)

getValue :: Vector Int -> Bool -> Int -> Int
getValue mem immediate pos =
  let memVal = mem ! pos
  in if (immediate)
       then memVal
       else mem ! memVal

step :: State -> Int -> State
step (State (mem, input, output)) pos
  -- let op = trace (show ((State (mem, input, output)), pos)) (mem ! pos)
 =
  let op = mem ! pos
  in case parseInstruction op of
       Add iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
             c = getValue mem True (pos + 3)
             r = a + b
         in step (State ((mem // [(c, r)]), input, output)) (pos + 4)
       Mult iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
             c = getValue mem True (pos + 3)
             r = a * b
         in step (State ((mem // [(c, r)]), input, output)) (pos + 4)
       Input ->
         let c = getValue mem True (pos + 1)
             r = head input
         in step (State ((mem // [(c, r)]), (tail input), output)) (pos + 2)
       Output imm ->
         let c = getValue mem imm (pos + 1)
         in step (State (mem, input, (c : output))) (pos + 2)
       JumpTrue iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
         in step
              (State (mem, input, output))
              (if (a /= 0)
                 then b
                 else pos + 3)
       JumpFalse iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
         in step
              (State (mem, input, output))
              (if (a == 0)
                 then b
                 else pos + 3)
       Less iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
             c = getValue mem True (pos + 3)
             r =
               if a < b
                 then 1
                 else 0
         in step (State ((mem // [(c, r)]), input, output)) (pos + 4)
       Equal iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
             c = getValue mem True (pos + 3)
             r =
               if a == b
                 then 1
                 else 0
         in step (State ((mem // [(c, r)]), input, output)) (pos + 4)
       Stop -> State (mem, input, output)

run :: String -> [Int] -> State
run str input =
  step (State (fromList (map read (splitOn "," str)), input, [])) 0

-- run2 :: String -> Int -> Int -> Int
-- run2 str noun verb =
--   let input = fromList (map read (splitOn "," str))
--   in (step (input // [(1, noun), (2, verb)]) 0) ! 0
ex1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"

input =
  "3,8,1001,8,10,8,105,1,0,0,21,42,59,76,85,106,187,268,349,430,99999,3,9,102,3,9,9,1001,9,2,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,102,3,9,9,101,3,9,9,1002,9,2,9,4,9,99,3,9,102,3,9,9,1001,9,4,9,1002,9,5,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,101,3,9,9,1002,9,2,9,1001,9,4,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99"

-- runStages :: Vector Int -> (Int, Int, Int, Int, Int) -> Int
runStages :: [Int] -> [Int] -> Int
runStages prog [a, b, c, d, e] =
  let State (_, _, o0) = step (State (fromList prog, [a, 0], [])) 0
      State (_, _, o1) = step (State (fromList prog, [b, head o0], [])) 0
      State (_, _, o2) = step (State (fromList prog, [c, head o1], [])) 0
      State (_, _, o3) = step (State (fromList prog, [d, head o2], [])) 0
      State (_, _, o4) = step (State (fromList prog, [e, head o3], [])) 0
  in head o4

-- pr (a, b) = run2 input a b == 19690720
main
  -- let prog = fromList (map read (splitOn "," ex1)) :: Vector Int
 = do
  let prog = (map read (splitOn "," input))
  putStrLn (show prog)
  let s = map (\c -> ((runStages prog c), c)) (permutations [0, 1, 2, 3, 4])
  putStrLn (show $ sortOn (\(a, _) -> a) s)
