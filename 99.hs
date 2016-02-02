import Control.Applicative
import Data.List
import System.Random

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (x:xs) = myLast xs
myButLast _ = error "list should have at least two elements"

elementAt :: Integral b => [a] -> b -> a
elementAt [] _ = error "index out of range"
elementAt (x:_) 1 = x
elementAt (x:xs) k = elementAt xs (k-1)

myLength :: Integral b => [a] -> b
myLength = foldl' (\acc x -> 1 + acc) 0

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (myReverse xs)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

compress :: Eq a => [a] -> [a]
compress (x:y:xs) | x == y = compress (x:xs)
                  | otherwise = x : compress (y:xs)
compress x = x

pack :: Eq a => [a] -> [[a]]
pack = foldr update []
    where update x acc | acc == [] = [[x]]
                       | x == ((head . head) acc) = (x : head acc) : tail acc
                       | otherwise = [x] : acc

encode :: (Eq a, Integral b) => [a] -> [(b, a)]
encode xs = map (\ys -> (myLength ys, head ys)) $ pack xs

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 1 = x : xs
insertAt x (y:ys) n = y : insertAt x ys (n-1)
insertAt _ [] _ = error "index out of range"

range :: Int -> Int -> Int
range i j = [i..j]

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = take n <$> map (\i -> xs !! i) <$> randomRs (0, max) <$> g
    where g = newStdGen
          max = length xs - 1

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = take n <$> randomRs (1, m) <$> newStdGen

combinations :: Int -> [a] -> [[a]]
combinations n _ | n < 0 = error "negative index"
combinations n xs | n > length xs = []
combinations 0 _ = []
combinations 1 xs = map (:[]) xs
combinations n (x:xs) = with ++ without
    where with = map (x:) $ combinations (n-1) xs
          without = combinations n xs