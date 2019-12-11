{-# LANGUAGE DeriveGeneric #-}

import           Data.Hashable
import           Data.HashMap.Strict (HashMap, empty, insert, lookupDefault,
                                      (!))
import           Data.List           (filter, find)
import           Data.List.Split     (splitOn)
import           GHC.Generics        (Generic)

import           Data.Maybe          (maybe)

--import           Data.Vector        (Vector, fromList, toList, (!), (//))
import           Debug.Trace         (trace)
import           System.Environment  (getArgs)

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

-- newtype Mem =
--   Mem (Vector Integer)
--   deriving (Eq, Ord, Show)
newtype Mem =
  Mem (HashMap Int Integer)
  deriving (Eq, Ord, Show)

data State =
  State Mem -- programm
        [Integer] -- input
        [Integer] -- output
        Int -- instruction pointer
        Int -- base
  deriving (Eq, Ord, Show)

fromIntegerList :: [Integer] -> Mem
fromIntegerList list =
  let map = foldr (\(k, v) -> insert k v) empty (zip [0 ..] list)
  in Mem map

(!?) :: Mem -> Int -> Integer
-- (!?) (Mem mem) k = mem ! k
(!?) (Mem mem) k = lookupDefault 0 k mem

getValue :: Mem -> Mode -> Int -> Int -> Integer
getValue mem (Mode mode) pos base =
  let memVal = mem !? pos
  in case mode of
       0 -> mem !? (fromInteger memVal)
       1 -> memVal
       2 -> mem !? (base + (fromInteger memVal))

setValue :: Mem -> Mode -> Int -> Int -> Integer -> Mem
setValue mem@(Mem map) (Mode mode) pos base value =
  let address =
        (if mode == 2
           then base
           else 0) +
        (fromInteger $ mem !? pos)
  in Mem (insert address value map)
  -- in Mem (vect // [(address, value)])

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

data Pos =
  Pos Int
      Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable Pos

data Hull =
  Hull (HashMap Pos Int)
  deriving (Eq, Ord, Show)

getColor :: Hull -> Pos -> Int
getColor (Hull hull) pos = lookupDefault 0 pos hull

setColor :: Hull -> Pos -> Int -> Hull
setColor (Hull hull) pos color = Hull (insert pos color hull)

{-|
data Hull =
  Hull [(Pos, Int)]
  deriving (Eq, Ord, Show)

getColor :: Hull -> Pos -> Int
getColor (Hull list) pos =
  let el = find (\(p, c) -> p == pos) list
  in maybe 0 (\(p, c) -> c) el

setColor :: Hull -> Pos -> Int -> Hull
setColor (Hull list) pos color = Hull ((pos, color) : list)
-}
{-|
setColor :: Hull -> Pos -> Int -> Hull
setColor (Hull list) pos color =
  let without = filter (\(p, c) -> p /= pos) list
  in Hull (without ++ [(pos, color)])
  -}
{-|
setColor :: Hull -> Pos -> Int -> Hull
setColor (Hull list) pos color =
  let el = find (\(p, c) -> p == pos) list
  in Hull
       (if (el == Nothing)
          then (list ++ [(pos, color)])
          else map
                 (\(p, c) ->
                    if p == pos
                      then (pos, color)
                      else (p, c))
                 list)
-}
input =
  "3,8,1005,8,352,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,28,1,1003,20,10,2,106,11,10,2,1107,1,10,1,1001,14,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,67,2,1009,7,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,101,0,8,92,1,105,9,10,1006,0,89,1,108,9,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,126,1,1101,14,10,1,1005,3,10,1006,0,29,1006,0,91,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,1002,8,1,161,1,1,6,10,1006,0,65,2,106,13,10,1006,0,36,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,198,1,105,15,10,1,1004,0,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,228,2,1006,8,10,2,1001,16,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,257,1006,0,19,2,6,10,10,2,4,13,10,2,1002,4,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,295,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,316,2,101,6,10,1006,0,84,2,1004,13,10,1,1109,3,10,101,1,9,9,1007,9,1046,10,1005,10,15,99,109,674,104,0,104,1,21101,387365315340,0,1,21102,369,1,0,1105,1,473,21101,666685514536,0,1,21102,380,1,0,1106,0,473,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,46266346536,1,21102,427,1,0,1105,1,473,21101,235152829659,0,1,21101,438,0,0,1105,1,473,3,10,104,0,104,0,3,10,104,0,104,0,21102,838337188620,1,1,21101,461,0,0,1105,1,473,21102,988753429268,1,1,21102,1,472,0,1106,0,473,99,109,2,22101,0,-1,1,21101,40,0,2,21101,504,0,3,21102,494,1,0,1106,0,537,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,499,500,515,4,0,1001,499,1,499,108,4,499,10,1006,10,531,1101,0,0,499,109,-2,2106,0,0,0,109,4,2101,0,-1,536,1207,-3,0,10,1006,10,554,21102,1,0,-3,21202,-3,1,1,21201,-2,0,2,21102,1,1,3,21101,573,0,0,1105,1,578,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,601,2207,-4,-2,10,1006,10,601,21201,-4,0,-4,1105,1,669,22101,0,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,620,0,0,1106,0,578,22102,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,639,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,661,22101,0,-1,1,21102,661,1,0,106,0,536,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0"

changeDir :: Int -> Char -> Char
changeDir 0 '^' = '<'
changeDir 0 '<' = 'v'
changeDir 0 'v' = '>'
changeDir 0 '>' = '^'
changeDir 1 '^' = '>'
changeDir 1 '<' = '^'
changeDir 1 'v' = '<'
changeDir 1 '>' = 'v'

move :: Pos -> Char -> Pos
move (Pos x y) '^' = Pos x (y - 1)
move (Pos x y) '<' = Pos (x - 1) y
move (Pos x y) 'v' = Pos x (y + 1)
move (Pos x y) '>' = Pos (x + 1) y

loop :: State -> Hull -> Pos -> Char -> Hull
loop state@(State mem input output p b) hull pos dir =
  let currentColor = toInteger $ getColor hull pos
      (State mem1 i1 [dirChange, col] p1 b1, running) =
        step (State mem [currentColor] [] p b)
  in if (not running)
       then hull
       else let newHull = setColor hull pos (fromInteger col)
                newDir = changeDir (fromInteger dirChange) dir
                newPos = move pos newDir
            in loop (State mem1 [] [] p1 b1) newHull newPos newDir

loop2 :: State -> Hull -> Pos -> Char -> Int -> Hull
loop2 state@(State mem input output p b) hull pos dir color =
  let currentColor = toInteger $ color --getColor hull pos
      (State mem1 i1 [dirChange, col] p1 b1, running) =
        step (State mem [currentColor] [] p b)
  in if (not running)
       then hull
       else let newHull = setColor hull pos (fromInteger col)
                newDir = changeDir (fromInteger dirChange) dir
                newPos = move pos newDir
                newColor = getColor newHull newPos
            in loop2 (State mem1 [] [] p1 b1) newHull newPos newDir newColor

run :: String -> Int
run str =
  let prog = (map read (splitOn "," str)) :: [Integer]
  -- in step (State (fromIntegerList prog) input [] 0 0)
      Hull hull =
        loop (State (fromIntegerList prog) [] [] 0 0) (Hull empty) (Pos 0 0) '^'
  in length hull
  -- in loop (State (fromIntegerList prog) [] [] 0 0) (Hull []) (Pos 0 0) '^'

run2 :: String -> Hull
run2 str =
  let prog = (map read (splitOn "," str)) :: [Integer]
  in loop2 (State (fromIntegerList prog) [] [] 0 0) (Hull empty) (Pos 0 0) '^' 1

printLine :: Hull -> Int -> String
printLine hull y =
  map
    (\x ->
       if (getColor hull (Pos x y) == 0)
         then '◽'
         else '◾')
    [0 .. 42]

printHull :: Hull -> String
printHull hull = unlines $ map (printLine hull) [0 .. 5]

main = do
  putStrLn (show $ run input)
  putStrLn (printHull $ run2 input)
