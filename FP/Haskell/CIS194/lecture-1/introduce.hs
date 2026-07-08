import GHC.Base (IO(IO))
int2Digit :: Integer -> [Integer]
int2Digit 0 = [0]
int2Digit x = go x []
    where
        go n acc 
            | n <= 0    = acc
            | otherwise = go n' (dig : acc)
                where 
                    n'  = n `div` 10
                    dig = n `mod` 10

myReverse :: [a] -> [a]
myReverse [] = []
myReverse x = go x []
    where 
        go :: [a] -> [a] -> [a]
        go [] a = a
        go (x:x') a = go x' $ x : a

doubleOther :: [Integer] -> [Integer]
doubleOther x = [if i `mod` 2 == 0 then a * 2 else a | (i, a) <- zip [1..] x]

-- 修正后的 validateId 
validateId :: [Integer] -> Bool
validateId x = go x 0 `mod` 10 == 0
    where
        go [] a = a
        go (val:xs) a 
            | val >= 10 = go xs (a + val - 9)  -- val >= 10 时，折算各位数
            | otherwise = go xs (a + val)      -- val < 10 时，正常累加（注意括号）

main :: IO Bool
main = (validateId . doubleOther . myReverse . int2Digit) <$> (readLn :: IO Integer)