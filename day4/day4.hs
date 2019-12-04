import           Data.List

pairwise :: [a] -> [(a, a)]
parwise [] = []

pairwise (_:[])  = []
pairwise (a:b:t) = (a, b) : (pairwise (b : t))

lessOrEq (a, b) = a <= b

same (a, b) = a == b

isCandidate :: Int -> Bool
isCandidate number =
  let str = show number
      pairs = pairwise str
  in (all lessOrEq pairs) && (any same pairs)

groupEq :: Eq a => [a] -> [[a]]
groupEq [] = []
groupEq (h:t) = groupEqHelper t [[h]]
  where
    groupEqHelper :: Eq a => [a] -> [[a]] -> [[a]]
    groupEqHelper [] acc = acc
    groupEqHelper (h:t) (accHead:accTail) =
      if h == (head accHead)
        then groupEqHelper t ((h : accHead) : accTail)
        else groupEqHelper t ([h] : accHead : accTail)

isCandidate2 :: Int -> Bool
isCandidate2 number =
  let str = show number
      gs = groupEq str
      groupLens = map length gs
  in elem 2 groupLens

solution1 = length $ filter isCandidate [356261 .. 846303]

solution2 = length $ filter isCandidate2 (filter isCandidate [356261 .. 846303])
