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

data State =
  State (Vector Int)
        [Int]
        [Int]
        Int
  deriving (Eq, Ord, Show)

getValue :: Vector Int -> Bool -> Int -> Int
getValue mem immediate pos =
  let memVal = mem ! pos
  in if (immediate)
       then memVal
       else mem ! memVal

step :: State -> (State, Bool)
step (State mem input output pos)
  -- let op = trace (show ((State (mem, input, output)), pos)) (mem ! pos)
 =
  let op = mem ! pos
  in case parseInstruction op of
       Add iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
             c = getValue mem True (pos + 3)
             r = a + b
         in step (State (mem // [(c, r)]) input output (pos + 4))
       Mult iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
             c = getValue mem True (pos + 3)
             r = a * b
         in step (State (mem // [(c, r)]) input output (pos + 4))
       Input ->
         if (input == [])
           then (State mem input output pos, True)
           else let c = getValue mem True (pos + 1)
                    r = head input
                in step (State (mem // [(c, r)]) (tail input) output (pos + 2))
       Output imm ->
         let c = getValue mem imm (pos + 1)
         in step (State mem input (c : output) (pos + 2))
       JumpTrue iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
         in step
              (State
                 mem
                 input
                 output
                 (if (a /= 0)
                    then b
                    else pos + 3))
       JumpFalse iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
         in step
              (State
                 mem
                 input
                 output
                 (if (a == 0)
                    then b
                    else pos + 3))
       Less iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
             c = getValue mem True (pos + 3)
             r =
               if a < b
                 then 1
                 else 0
         in step (State (mem // [(c, r)]) input output (pos + 4))
       Equal iA iB ->
         let a = getValue mem iA (pos + 1)
             b = getValue mem iB (pos + 2)
             c = getValue mem True (pos + 3)
             r =
               if a == b
                 then 1
                 else 0
         in step (State (mem // [(c, r)]) input output (pos + 4))
       Stop -> ((State mem input output pos), False)

-- run :: String -> [Int] -> State
-- run str input = step (State (fromList (map read (splitOn "," str))) input [] 0)
ex1 =
  "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

input =
  "3,8,1001,8,10,8,105,1,0,0,21,42,59,76,85,106,187,268,349,430,99999,3,9,102,3,9,9,1001,9,2,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,102,3,9,9,101,3,9,9,1002,9,2,9,4,9,99,3,9,102,3,9,9,1001,9,4,9,1002,9,5,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,101,3,9,9,1002,9,2,9,1001,9,4,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99"

-- runStages :: Vector Int -> (Int, Int, Int, Int, Int) -> Int
runStages :: [Int] -> [Int] -> Int
runStages prog [a, b, c, d, e] =
  let (State _ _ o0 pos0, running0) = step (State (fromList prog) [a, 0] [] 0)
      (State _ _ o1 pos1, running1) =
        step (State (fromList prog) [b, head o0] [] 0)
      (State _ _ o2 pos2, running2) =
        step (State (fromList prog) [c, head o1] [] 0)
      (State _ _ o3 pos3, running3) =
        step (State (fromList prog) [d, head o2] [] 0)
      (State _ _ o4 pos4, running4) =
        step (State (fromList prog) [e, head o3] [] 0)
  in head o4

stepWithEvent :: State -> Int -> (State, Int, Bool)
stepWithEvent (State mem input output pos) event =
  let (State mem0 input0 (out:output0) pos0, running0) =
        step (State mem (input ++ [event]) output pos)
  in ((State mem0 input0 output0 pos0), out, running0)

runStageRound :: [State] -> Int -> Int
runStageRound [a, b, c, d, e] event =
  let (state0, output0, running0) = stepWithEvent a event
      (state1, output1, running1) = stepWithEvent b output0
      (state2, output2, running2) = stepWithEvent c output1
      (state3, output3, running3) = stepWithEvent d output2
      (state4, output4, running4) = stepWithEvent e output3
  in if (running4)
       then runStageRound [state0, state1, state2, state3, state4] output4
       else output4

runStageRounds :: [Int] -> [Int] -> Int
runStageRounds prog [a, b, c, d, e] =
  runStageRound
    [ State (fromList prog) [a] [] 0
    , State (fromList prog) [b] [] 0
    , State (fromList prog) [c] [] 0
    , State (fromList prog) [d] [] 0
    , State (fromList prog) [e] [] 0
    ]
    0

main = do
  let prog = (map read (splitOn "," input))
  putStrLn (show prog)
  let s = map (\c -> ((runStageRounds prog c), c)) (permutations [5 .. 9])
  putStrLn (show $ sortOn (\(a, _) -> a) s)
