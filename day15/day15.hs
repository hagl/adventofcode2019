{-# LANGUAGE DeriveGeneric #-}

import           Data.Hashable
import           Data.HashMap.Strict (HashMap, elems, empty, insert, keys,
                                      lookupDefault, map, toList, (!))
import           Data.List           (filter, find, groupBy, sort)
import           Data.List.Split     (chunksOf, splitOn)
import           GHC.Generics        (Generic)

import           Data.Maybe          (maybe)

import           Data.Vector         (Vector, fromList, (!), (//))
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

input =
  "3,1033,1008,1033,1,1032,1005,1032,31,1008,1033,2,1032,1005,1032,58,1008,1033,3,1032,1005,1032,81,1008,1033,4,1032,1005,1032,104,99,1002,1034,1,1039,102,1,1036,1041,1001,1035,-1,1040,1008,1038,0,1043,102,-1,1043,1032,1,1037,1032,1042,1106,0,124,1002,1034,1,1039,101,0,1036,1041,1001,1035,1,1040,1008,1038,0,1043,1,1037,1038,1042,1105,1,124,1001,1034,-1,1039,1008,1036,0,1041,102,1,1035,1040,1002,1038,1,1043,1002,1037,1,1042,1106,0,124,1001,1034,1,1039,1008,1036,0,1041,1002,1035,1,1040,101,0,1038,1043,1002,1037,1,1042,1006,1039,217,1006,1040,217,1008,1039,40,1032,1005,1032,217,1008,1040,40,1032,1005,1032,217,1008,1039,9,1032,1006,1032,165,1008,1040,3,1032,1006,1032,165,1102,2,1,1044,1105,1,224,2,1041,1043,1032,1006,1032,179,1101,1,0,1044,1106,0,224,1,1041,1043,1032,1006,1032,217,1,1042,1043,1032,1001,1032,-1,1032,1002,1032,39,1032,1,1032,1039,1032,101,-1,1032,1032,101,252,1032,211,1007,0,29,1044,1105,1,224,1101,0,0,1044,1105,1,224,1006,1044,247,102,1,1039,1034,1002,1040,1,1035,1001,1041,0,1036,1002,1043,1,1038,102,1,1042,1037,4,1044,1106,0,0,19,27,41,9,17,87,2,1,91,14,15,99,17,13,40,13,7,33,23,28,7,21,75,15,41,83,18,4,28,1,21,99,3,2,4,60,16,5,16,22,59,18,37,21,62,96,11,63,46,16,27,76,7,36,38,28,53,18,84,52,12,47,25,93,10,57,64,21,41,75,52,9,80,60,21,86,60,21,70,21,13,72,78,22,61,17,28,54,51,93,18,3,87,21,4,98,17,59,2,17,18,71,5,20,16,39,66,18,7,62,15,37,25,52,27,17,15,10,48,11,39,18,20,68,83,22,36,9,3,69,56,64,21,39,93,1,90,18,57,52,14,41,32,57,5,7,72,18,35,66,21,22,88,2,31,52,7,35,25,50,14,35,7,11,92,38,14,66,3,28,84,18,17,48,15,34,40,4,21,92,52,27,5,4,53,65,59,24,88,24,66,88,85,26,8,26,10,64,99,9,44,38,14,26,74,75,24,31,7,6,62,9,57,75,18,22,52,57,15,3,87,21,39,24,12,8,70,8,19,3,89,16,36,15,36,16,30,28,8,89,12,99,98,16,78,24,11,63,87,55,51,19,57,18,28,9,90,15,95,56,57,1,93,77,24,36,14,44,46,25,66,37,23,8,12,10,58,27,66,4,72,1,2,16,91,16,66,26,24,53,25,20,41,8,75,23,2,20,91,19,3,12,32,30,3,33,85,17,21,92,17,1,12,73,9,34,12,85,42,5,69,67,4,87,70,6,49,96,12,5,37,62,54,72,13,52,14,21,84,68,54,22,78,11,93,12,90,55,7,19,44,21,98,4,46,50,27,30,2,99,27,35,8,5,62,1,91,65,12,80,16,17,81,14,73,60,69,24,23,13,74,57,10,26,21,80,60,10,79,3,9,37,77,73,16,10,3,13,95,4,91,65,11,86,16,24,71,22,6,63,90,56,15,64,8,25,46,77,71,24,13,72,96,22,8,15,79,39,19,19,47,14,16,92,69,73,23,76,23,28,60,84,14,54,62,11,8,30,75,44,16,4,30,82,14,80,11,1,70,85,10,14,73,70,9,54,25,26,12,51,23,86,92,18,11,19,74,55,51,10,73,7,13,43,89,5,55,2,18,82,2,14,63,71,28,7,94,61,10,51,8,53,63,22,39,19,79,20,99,2,66,22,7,68,71,17,19,45,10,14,42,99,9,9,13,75,84,14,83,75,19,92,22,47,4,83,18,46,91,22,61,28,6,71,17,10,1,81,6,60,83,21,14,13,71,11,68,73,52,10,25,30,91,6,25,86,89,19,39,18,95,1,52,23,91,20,14,41,91,26,59,16,85,99,4,15,96,51,19,25,51,73,3,48,79,14,14,41,5,17,59,8,51,43,21,15,47,3,28,53,12,22,23,2,94,74,23,53,20,20,98,21,14,46,61,26,6,55,20,69,28,6,41,19,70,48,6,9,32,32,28,20,21,62,22,38,7,90,3,32,24,92,49,23,72,63,17,18,89,85,33,28,23,27,5,42,52,7,54,18,17,21,63,98,8,9,84,31,24,80,70,22,51,28,61,77,6,25,68,66,8,47,22,7,44,26,37,15,28,68,23,18,18,14,34,3,85,99,31,41,53,28,20,43,90,22,13,70,27,27,17,35,48,11,92,4,60,84,4,38,27,25,89,99,74,2,31,63,13,50,1,54,4,59,3,59,2,54,15,37,19,74,45,75,7,84,19,96,72,75,9,34,18,52,23,99,11,45,81,53,7,71,24,80,26,31,11,74,27,57,0,0,21,21,1,10,1,0,0,0,0,0,0"

run :: String -> [Integer] -> (State, Bool)
run str input =
  let prog = (Prelude.map read (splitOn "," str)) :: [Integer]
  in step (State (fromIntegerList prog) input [] 0 0)

loop2 state@(State mem input output p b) =
  let (State mem1 i1 o1 p1 b1, running) = step (State mem input output p b)
     --trace (show o1) $
  in if (not running)
       then o1
       else let or = Prelude.map reverse $ chunksOf 3 o1
                p = findX 3 or
                b = findX 4 or
                d = signum (b - p)
            in loop2 (State mem1 [fromIntegral d] o1 p1 b1)

runGame :: String -> [Integer] -> [[Integer]]
runGame str input =
  let prog = (Prelude.map read (splitOn "," str)) :: [Integer]
      (State _ _ output _ _, _) =
        step (State (fromIntegerList prog) input [] 0 0)
  in chunksOf 3 $ reverse output

-- playGame :: String -> [Integer] -> [Integer]
playGame str input =
  let prog = (Prelude.map read (splitOn "," str)) :: [Integer]
  in loop2 (State (fromIntegerList prog) [] [] 0 0)

data Pos =
  Pos Int
      Int
  deriving (Eq, Ord, Show, Generic)

instance Hashable Pos

data Hull =
  Hull (HashMap Pos (Int, Int))
  deriving (Eq, Ord, Show)

getHullValue :: Hull -> Pos -> (Int, Int)
getHullValue (Hull hull) pos = lookupDefault (-1, -1) pos hull

setHullValue :: Hull -> Pos -> (Int, Int) -> Hull
setHullValue (Hull hull) pos color = Hull (insert pos color hull)

movePos :: Pos -> Int -> Pos
movePos (Pos x y) 1 = Pos x (y - 1)
movePos (Pos x y) 2 = Pos x (y + 1)
movePos (Pos x y) 3 = Pos (x - 1) y
movePos (Pos x y) 4 = Pos (x + 1) y

explore :: Int -> Pos -> Hull -> State -> (Hull, State)
explore currentStep currentPos hull state =
  let (hull1, state1) = exploreDir 1 2 currentStep currentPos hull state
      (hull2, state2) = exploreDir 2 1 currentStep currentPos hull1 state1
      (hull3, state3) = exploreDir 3 4 currentStep currentPos hull2 state2
      (hull4, state4) = exploreDir 4 3 currentStep currentPos hull3 state3
  in (hull4, state4)

tryMove :: State -> Int -> (State, Int)
tryMove (State mem input output p b) dir =
  let (newState@(State _ _ (val:_) _ _), _) =
        step (State mem ((fromIntegral dir) : input) output p b)
  in (newState, fromInteger val)

exploreDir :: Int -> Int -> Int -> Pos -> Hull -> State -> (Hull, State)
exploreDir dir back currentStep currentPos hull state =
  let newPos = movePos currentPos dir
      (val, steps) = getHullValue hull newPos
  in if (val == -1) || ((val /= 0) && (steps > (currentStep + 1)))
       then let (newState, val) = tryMove state dir
                newHull = setHullValue hull newPos (val, currentStep + 1)
            in case val of
                 0 -> (newHull, newState)
                 _ ->
                   let (exploredHull, exploredState) =
                         explore (currentStep + 1) newPos newHull newState
                       (backState, _) = tryMove exploredState back
                   in (exploredHull, backState)
       else (hull, state)

getGrid :: String -> Hull
getGrid str =
  let prog = (Prelude.map read (splitOn "," str)) :: [Integer]
      state = (State (fromIntegerList prog) [] [] 0 0)
      pos = Pos 0 0
      hull = setHullValue (Hull empty) pos (5, 0)
      (newHull, _) = explore 0 pos hull state
  in newHull

getSteps :: String -> Int
getSteps input =
  let Hull hull = getGrid input
      Just (_, steps) = find (\(val, _) -> val == 2) $ elems hull
  in steps

fill :: Int -> Pos -> Hull -> Hull
fill currentStep currentPos hull =
  let hull1 = fillDir 1 currentStep currentPos hull
      hull2 = fillDir 2 currentStep currentPos hull1
      hull3 = fillDir 3 currentStep currentPos hull2
      hull4 = fillDir 4 currentStep currentPos hull3
  in hull4

fillDir :: Int -> Int -> Pos -> Hull -> Hull
fillDir dir currentStep currentPos hull =
  let newPos = movePos currentPos dir
      (val, steps) = getHullValue hull newPos
  in if ((val /= 0) && (steps > (currentStep + 1)))
       then let newHull = setHullValue hull newPos (val, currentStep + 1)
                filledHull = fill (currentStep + 1) newPos newHull
            in filledHull
       else hull

getSteps2 :: String -> Int
getSteps2 input =
  let Hull hull = getGrid input
      Just (startPos, _) = find (\(_, (val, _)) -> val == 2) $ toList hull
      newHull =
        setHullValue
          (Hull
             (Data.HashMap.Strict.map
                (\(val, steps) ->
                   ( val
                   , if val == 0
                       then 0
                       else 1000000))
                hull))
          startPos
          (2, 0)
      Hull filledHull = fill 0 startPos newHull
  in maximum $ Prelude.map (\(val, steps) -> steps) $ elems filledHull

main = do
  putStrLn (printHull $ getGrid input)
  putStrLn (show $ getSteps input)
  putStrLn (show $ getSteps2 input)

printHull :: Hull -> String
printHull (Hull hull) =
  let ps = keys hull
      xs = Prelude.map (\(Pos x _) -> x) ps
      ys = Prelude.map (\(Pos _ y) -> y) ps
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
  in printGrid [minX .. maxX] [minY .. maxY] (getHullColor (Hull hull))

findX :: Integer -> [[Integer]] -> Integer
findX v list =
  case find (\[a, b, c] -> c == v) list of
    Just [a, b, c] -> a
    _              -> error ("finX - " ++ (show v) ++ " : " ++ (show list))

printGrid :: [Int] -> [Int] -> (Int -> Int -> Char) -> String
printGrid dimX dimY getColor = unlines $ Prelude.map (printLine) dimY
  where
    printLine :: Int -> String
    printLine y = Prelude.map (\x -> getColor x y) dimX

getHullColor :: Hull -> Int -> Int -> Char
getHullColor hull x y =
  let (val, step) = getHullValue hull (Pos x y)
  in case val of
       0    -> '#'
       1    -> '.'
       (-1) -> ' '
       2    -> '*'
       5    -> 'D'

getColor :: [[Integer]] -> Int -> Int -> Char
getColor list x y =
  let c = find (\[a, b, c] -> (fromInteger a) == x && (fromInteger b) == y) list
  in maybe
       ' '
       (\[_, _, x] ->
          case x of
            0 -> ' '
            1 -> '◾'
            2 -> '#'
            3 -> '-'
            4 -> 'O')
       c
