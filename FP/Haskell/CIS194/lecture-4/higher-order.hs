import Data.List (unfoldr)
-- Practicing applying Wholemeal Programming

-- my foldr

import Data.Monoid (Endo(..))

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f base xs = appEndo (foldMap (\x -> Endo (\y -> f x y)) xs) base

-- 原型
fun1' :: [Integer] -> Integer

fun1' [] = 1
fun1' (x:xs) 
    | even x = (x - 2) * fun1' xs
    | otherwise = fun1' xs

-- 改进
fun1 :: [Integer] -> Integer

fun1 x = foldr (*) 1 . map ((-) 2) . filter even $ x



fun2 :: Integer -> Integer

fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- 改进
fun2' :: Integer -> Integer

fun2' = sum . takeWhile (>1) . iterate nextStep
    where
        nextStep n = if even n then n `div` 2 else 3 * n + 1

-- 另一种
fun2'' :: Integer -> Integer

fun2'' = sum . unfoldr step
    where
        step 1 = Nothing
        step n = Just (n, if even n then n `div` 2 else 3 * n + 1)


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a

foldTree = foldr insert Leaf

makeNode:: a -> Tree a
makeNode x = Node 0 Leaf x Leaf

height :: Tree a -> Integer
height Leaf = -1
height (Node x _ _ _) = x

reBuild :: Tree a -> a -> Tree a -> Tree a
reBuild l root r = Node (max (height l) (height r) + 1) l root r


insert :: a -> Tree a -> Tree a 
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node _ left root right)
    | height left <= height right = reBuild (insert a left) root right
    | otherwise = reBuild left root (insert a right)

-- More folds

xor :: [Bool] -> Bool
xor  = foldr (/=) False 

map' :: (a -> b) -> [a] -> [b]
map' f l = foldr (\x y -> (f x): y) [] l

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x packed y -> packed (f y x)) id xs base

-- f [1 2 3]

-- generate odd primes

sieveSundaram::Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) remaining
    where
        remaining = filter (`notElem` removed) [1 .. n]
        removed = [i + j + 2 * i * j
                  | i <- [1 .. n], j <- [i .. n]
                  , i + j + 2 * i * j <= n]