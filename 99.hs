import Control.Applicative
import Data.List
import System.Random hiding (split)

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

data Occurence a = Multiple Int a | Single a deriving Show
encodeModified :: Eq a => [a] -> [Occurence a]
encodeModified xs = map tupleToOccurence $ encode xs
    where tupleToOccurence (1, x) = Single x
          tupleToOccurence (n, x) = Multiple n x

decodeModified :: [Occurence a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of
    (Single y) -> y : decodeModified xs
    (Multiple n y) -> replicate n y ++ decodeModified xs

encodeDirect :: Eq a => [a] -> [Occurence a]
encodeDirect = foldr update []
    where update x acc = case acc of
              [] -> [Single x]
              (Single y) : as | x == y -> (Multiple 2 x) : as
              (Multiple n y) : as | x == y -> (Multiple (n+1) x) : as
              _ -> (Single x) : acc

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter (\(_, s) -> s `mod` n /= 0) $ zip xs [1 .. ]

split :: [a] -> Int -> ([a], [a])
split [] _ = error "empty list"
split xs 0 = ([], xs)
split (x:xs) n = (x:f, s)
    where (f, s) = split xs (n-1)

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j - i + 1) $ drop (i - 1) xs

rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs n | 0 <= n && n < len = drop n xs ++ take n xs
            | otherwise = rotate xs $ n `mod` len
            where len = length xs

removeAt :: Int -> [a] -> [a]
removeAt _ [] = error "index out of range"
removeAt 1 (x:xs) = xs
removeAt n (x:xs) = x : removeAt (n - 1) xs

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 1 = x : xs
insertAt x (y:ys) n = y : insertAt x ys (n-1)
insertAt _ [] _ = error "index out of range"

range :: Int -> Int -> [Int]
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
