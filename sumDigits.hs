divideByTen :: Integer -> Integer
divideByTen = floor . realToFrac . (/10) . fromIntegral

toDigits :: Integer -> [Integer]
toDigits x  
    | x < 10 = [x] 
    | otherwise = (toDigits $ divideByTen x) ++ [mod x 10]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum $ toDigits x
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs 
