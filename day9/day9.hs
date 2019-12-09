import           Data.List
import           Data.List.Split    (splitOn)
import           Data.Vector        (Vector, fromList, toList, (!), (//))
import           Debug.Trace        (trace)
import           System.Environment (getArgs)

newtype Mode =
  Mode Int
  deriving (Eq, Ord, Show)

data Instruction
  = Add Mode
        Mode
        Mode
  | Mult Mode
         Mode
         Mode
  | Input Mode
  | Output Mode
  | JumpTrue Mode
             Mode
  | JumpFalse Mode
              Mode
  | Less Mode
         Mode
         Mode
  | Equal Mode
          Mode
          Mode
  | Stop
  | Adjust Mode
  deriving (Eq, Ord, Show)

parseInstruction :: Integer -> Instruction
parseInstruction opCode =
  let number = fromInteger opCode
      op = number `mod` 100
      modeC = Mode $ (number `div` 100) `mod` 10
      modeB = Mode $ (number `div` 1000) `mod` 10
      modeA = Mode $ (number `div` 10000) `mod` 10
  in case mod number 100 of
       01 -> Add modeC modeB modeA
       02 -> Mult modeC modeB modeA
       03 -> Input modeC
       04 -> Output modeC
       05 -> JumpTrue modeC modeB
       06 -> JumpFalse modeC modeB
       07 -> Less modeC modeB modeA
       08 -> Equal modeC modeB modeA
       09 -> Adjust modeC
       99 -> Stop
       _  -> error "Error"

newtype Mem =
  Mem (Vector Integer)
  deriving (Eq, Ord, Show)

data State =
  State Mem -- programm
        [Integer] -- input
        [Integer] -- output
        Int -- instruction pointer
        Int -- base
  deriving (Eq, Ord, Show)

fromIntegerList :: [Integer] -> Mem
fromIntegerList list = Mem (fromList list)

getValue :: Mem -> Mode -> Int -> Int -> Integer
getValue (Mem mem) (Mode mode) pos base =
  let memVal = mem ! pos
  in case mode of
       0 -> mem ! (fromInteger memVal)
       1 -> memVal
       2 -> mem ! (base + (fromInteger memVal))

setValue :: Mem -> Mode -> Int -> Int -> Integer -> Mem
setValue mem@(Mem vect) mode pos base value =
  let address = fromInteger $ getValue mem mode pos base
  in Mem (vect // [(address, value)])

step :: State -> (State, Bool)
step state@(State mem input output pos base)
  -- let op = trace (show ((State (mem, input, output)), pos)) (mem ! pos)
  -- let op = mem ! pos
 =
  let op = getValue mem (Mode 1) pos 0
  in case parseInstruction op of
       Add iA iB iC ->
         let a = getValue mem iA (pos + 1) base
             b = getValue mem iB (pos + 2) base
             r = a + b
             newMem = setValue mem iC (pos + 3) base r
         in step (State newMem input output (pos + 4) base)
       Mult iA iB iC ->
         let a = getValue mem iA (pos + 1) base
             b = getValue mem iB (pos + 2) base
             r = a * b
             newMem = setValue mem iC (pos + 3) base r
         in step (State newMem input output (pos + 4) base)
       Input mode ->
         if (input == [])
           then (State mem input output pos base, True)
           else let r = head input
                    newMem = setValue mem mode (pos + 1) base r
                in step (State newMem (tail input) output (pos + 2) base)
       Output mode ->
         let c = getValue mem mode (pos + 1) base
         in step (State mem input (c : output) (pos + 2) base)
       JumpTrue iA iB ->
         let a = getValue mem iA (pos + 1) base
             b = getValue mem iB (pos + 2) base
         in step
              (State
                 mem
                 input
                 output
                 (if (a /= 0)
                    then fromInteger b
                    else pos + 3)
                 base)
       JumpFalse iA iB ->
         let a = getValue mem iA (pos + 1) base
             b = getValue mem iB (pos + 2) base
         in step
              (State
                 mem
                 input
                 output
                 (if (a == 0)
                    then fromInteger b
                    else pos + 3)
                 base)
       Less iA iB iC ->
         let a = getValue mem iA (pos + 1) base
             b = getValue mem iB (pos + 2) base
             r =
               if a < b
                 then 1
                 else 0
             newMem = setValue mem iC (pos + 3) base r
         in step (State newMem input output (pos + 4) base)
       Equal iA iB iC ->
         let a = getValue mem iA (pos + 1) base
             b = getValue mem iB (pos + 2) base
             r =
               if a == b
                 then 1
                 else 0
             newMem = setValue mem iC (pos + 3) base r
         in step (State newMem input output (pos + 4) base)
       Stop -> ((State mem input output pos base), False)
       Adjust mode ->
         let a = getValue mem mode (pos + 1) base
         in step (State mem input output (pos + 2) (base + (fromInteger a)))

-- run :: String -> [Int] -> State
-- run str input = step (State (fromList (map read (splitOn "," str))) input [] 0)
ex1 = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"

ex2 = "1102,34915192,34915192,7,4,7,99,0"

ex3 = "104,1125899906842624,99"

input =
  "3,8,1001,8,10,8,105,1,0,0,21,42,59,76,85,106,187,268,349,430,99999,3,9,102,3,9,9,1001,9,2,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,102,3,9,9,101,3,9,9,1002,9,2,9,4,9,99,3,9,102,3,9,9,1001,9,4,9,1002,9,5,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,101,3,9,9,1002,9,2,9,1001,9,4,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99"

run :: String -> [Integer] -> (State, Bool)
run str input =
  let prog = (map read (splitOn "," str)) :: [Integer]
  in step (State (fromIntegerList prog) input [] 0 0)

main = do
  putStrLn (show (run input [1]))
