import Data.List (unfoldr)
-- Practicing applying Wholemeal Programming

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

makeNode:: a -> Tree a
makeNode x = Node 0 Leaf x Leaf

insert :: a -> Tree a -> Tree a 
insert a (_ left root right) = 

foldTree = foldr insert Leaf
    where
        insert value Leaf = Node 0 Leaf value Leaf
        insert value (Node _ left root right)
            | height left <= height right = makeNode left' root right
            | otherwise = makeNode left root right'
            where
                left' = insert value left
                right' = insert value right

        makeNode left root right =
            Node (1 + max (height left) (height right)) left root right

        height Leaf = -1
        height (Node value _ _ _) = value