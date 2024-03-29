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
ex1 = "1002,4,3,4,33"

input =
  "3,225,1,225,6,6,1100,1,238,225,104,0,1101,48,82,225,102,59,84,224,1001,224,-944,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1101,92,58,224,101,-150,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,10,89,224,101,-890,224,224,4,224,1002,223,8,223,1001,224,5,224,1,224,223,223,1101,29,16,225,101,23,110,224,1001,224,-95,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1102,75,72,225,1102,51,8,225,1102,26,16,225,1102,8,49,225,1001,122,64,224,1001,224,-113,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1102,55,72,225,1002,174,28,224,101,-896,224,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1102,57,32,225,2,113,117,224,101,-1326,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1,148,13,224,101,-120,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,677,226,224,102,2,223,223,1006,224,329,101,1,223,223,107,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,8,226,677,224,102,2,223,223,1006,224,359,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,374,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,107,677,226,224,102,2,223,223,1006,224,404,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,419,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,434,1001,223,1,223,1008,677,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,7,226,677,224,1002,223,2,223,1006,224,464,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,479,1001,223,1,223,1007,226,226,224,1002,223,2,223,1005,224,494,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1007,226,677,224,1002,223,2,223,1006,224,524,101,1,223,223,1107,677,677,224,102,2,223,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,569,1001,223,1,223,1108,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,8,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,644,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,659,101,1,223,223,1108,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226" -- pr :: (Int, Int) -> Bool

-- pr (a, b) = run2 input a b == 19690720
main = do
  putStrLn (show (run input [1]))
  putStrLn (show (run input [5]))
    -- putStrLn (show (find pr [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]))
