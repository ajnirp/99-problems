-- Eight queens problem

-- add in an element x at every position in the list xs
addIn :: a -> [a] -> [[a]]
addIn x xs = map (\i -> take i xs ++ [x] ++ drop i xs) [0..(length xs)]

-- generate all permutations of the list [1..n]
perms :: Int -> [[Int]]
perms n = helper [1..n]
    where helper :: [a] -> [[a]]
          helper [] = []
          helper [x] = [[x]]
          helper (x:xs) = concatMap (addIn x) $ helper xs

-- does one position *diagonally* attack the other?
attacks :: (Int, Int) -> (Int, Int) -> Bool
attacks (a, b) (x, y) = abs (a - x) == abs (b - y)

-- is an arrangement of queens valid? in the list xs,
-- the value of ith element is the row in which the queen in column i is located
isValid :: [Int] -> Bool
isValid xs = helper $ zip [1..] xs
    where helper :: [(Int, Int)] -> Bool
          helper [] = True
          helper (x:xs) = xDoesntAttack && noOtherAttacks
              where xDoesntAttack = all (== False) $ map (x `attacks`) xs
                    noOtherAttacks = helper xs

answer :: [[Int]]
answer = filter isValid $ perms 8