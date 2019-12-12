import           Data.List          (concatMap, elemIndex, find, sort, sortOn)
import           Data.Maybe         (fromJust)
import           Data.Set           (Set, empty, insert, member)
import           Debug.Trace        (trace)
import           System.Environment (getArgs)

data Pos =
  Pos Int
      Int
      Int
  deriving (Eq, Ord, Show)

data Vel =
  Vel Int
      Int
      Int
  deriving (Eq, Ord, Show)

updateVel :: Vel -> [Pos] -> Pos -> Vel
updateVel vel positions (Pos x y z) =
  foldl
    (\(Vel vx vy vz) ->
       \(Pos x' y' z') ->
         Vel
           (vx + signum (x' - x))
           (vy + signum (y' - y))
           (vz + signum (z' - z)))
    vel
    positions

updateVel1 :: Int -> [Int] -> Int -> Int
updateVel1 vel positions x =
  foldl (\vx -> \x' -> vx + signum (x' - x)) vel positions

updatePos :: Pos -> Vel -> Pos
updatePos (Pos x y z) (Vel dx dy dz) = Pos (x + dx) (y + dy) (z + dz)

updatePos1 :: Int -> Int -> Int
updatePos1 = (+)

updateMoon :: [Moon] -> Moon -> Moon
updateMoon moons (Moon pos vel) =
  let positions = map (\(Moon p v) -> p) moons
      newVel = updateVel vel positions pos
      newPos = updatePos pos newVel
  in Moon newPos newVel

updateMoon1 :: [M1] -> M1 -> M1
updateMoon1 moons (M1 pos vel) =
  let positions = map (\(M1 p v) -> p) moons
      newVel = updateVel1 vel positions pos
      newPos = updatePos1 pos newVel
  in M1 newPos newVel

updateMoons :: [Moon] -> [Moon]
updateMoons moons = map (updateMoon moons) moons

updateMoons1 :: [M1] -> [M1]
updateMoons1 moons = map (updateMoon1 moons) moons

energy :: Moon -> Int
energy (Moon (Pos x y z) (Vel dx dy dz)) =
  ((abs x) + (abs y) + (abs z)) * ((abs dx) + (abs dy) + (abs dz))

data Moon =
  Moon Pos
       Vel
  deriving (Eq, Ord, Show)

data M1 =
  M1 Int
     Int
  deriving (Eq, Ord, Show)

ex1 = [Pos (-1) 0 2, Pos 2 (-10) (-7), Pos 4 (-8) 8, Pos 3 5 (-1)]

ex2 = [Pos (-8) (-10) 0, Pos 5 5 10, Pos 2 (-7) 3, Pos 9 (-8) (-3)]

input = [Pos (-1) 7 3, Pos 12 2 (-13), Pos 14 18 (-8), Pos 17 4 (-4)]

initial :: [Pos] -> [Moon]
initial positions = map (\p -> Moon p (Vel 0 0 0)) positions

initial1 :: [Int] -> [M1]
initial1 positions = map (\p -> M1 p 0) positions

x1 :: Pos -> Int
x1 (Pos x _ _) = x

y1 :: Pos -> Int
y1 (Pos _ y _) = y

z1 :: Pos -> Int
z1 (Pos _ _ z) = z

runN :: Int -> [Moon] -> [Moon]
runN n moons = foldr (\i -> updateMoons) moons [1 .. n]

data Cache =
  Cache (Set [Moon])

findDuplicate :: Int -> [Moon] -> Cache -> Int
findDuplicate n moons (Cache cache) =
  let updatedCache = insert moons cache
      newMoons = updateMoons moons
      nextN =
        if (mod n 1000 == 0)
          then trace (show n) (n + 1)
          else (n + 1)
  in if (member newMoons updatedCache)
       then (n + 1)
       else findDuplicate nextN newMoons (Cache updatedCache)

findDuplicate2 :: Int -> [Moon] -> [Moon] -> Int
findDuplicate2 n moons initialPos =
  let newMoons = updateMoons moons
  in if (newMoons == initialPos)
       then (n + 1)
       else findDuplicate2 (n + 1) newMoons initialPos

{-|
findDuplicate1 :: Int -> [M1] -> [[M1]] -> Int
findDuplicate1 n moons cache =
  let newMoons = updateMoons1 moons
      index = elemIndex newMoons cache
  in if (index == Nothing)
       then findDuplicate1 (n + 1) newMoons cache
       else fromJust index
-}
findDuplicate1 :: Int -> [M1] -> (Set [M1]) -> Int
findDuplicate1 n moons cache =
  let updatedCache = insert moons cache
      newMoons = updateMoons1 moons
  in if (member newMoons updatedCache)
       then (n + 1)
       else findDuplicate1 (n + 1) newMoons updatedCache

findDuplicate' :: [Pos] -> Int
findDuplicate' inp =
  let cx = findDuplicate1 0 (initial1 $ map x1 inp) empty
      cy = findDuplicate1 0 (initial1 $ map y1 inp) empty
      cz = findDuplicate1 0 (initial1 $ map z1 inp) empty
  in lcm cx $ lcm cy cz

main = do
  putStrLn (show $ sum $ map energy $ runN 1000 $ initial input)
  -- putStrLn (show $ (findDuplicate 0 (initial ex1) (Cache empty)))
  -- putStrLn (show $ (findDuplicate2 0 (initial ex1) (initial ex1)))
  putStrLn (show $ findDuplicate' ex1)
  putStrLn (show $ findDuplicate' input)
  -- putStrLn (show $ (findDuplicate2 0 (initial input) (initial input)))
