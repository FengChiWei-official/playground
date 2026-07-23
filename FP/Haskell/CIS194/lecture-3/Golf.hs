module Golf where

filterBy :: Int -> [a] -> [a]
filterBy n xs = [v | (i, v) <- zip [1..] xs, i `mod` n == 0]


skips :: [a] -> [[a]]
skips x = [filterBy n x | n <- [1 .. length x]]


localMaxima :: [Integer] -> [Integer]
localMaxima xs = [y | (x, y, z) <- zip3 xs (drop 1 xs) (drop 2 xs), y > x && y > z]

basicString::String
basicString = "==========\n0123456789\n"

statistic :: [Integer] -> [Integer]
statistic xs = [toInteger . length $ filter (i ==) xs | i <- [1..9]]

histogram :: [Integer]->String
histogram xs = generate xs ++ basicString
    where 
        toStar val
            | val > 0 = '*'
            | otherwise = ' '

        generate:: [Integer] -> String
        generate list
            | any (>0) list = (generate $ map (\x -> max 0 x-1) list) ++ (map toStar list ++ ['\n'])
            | otherwise = ""