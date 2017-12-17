--Break an Integer into [Integer]
divideByTen :: Integer -> Integer
divideByTen = floor . realToFrac . (/10) . fromIntegral

toDigits :: Integer -> [Integer]
toDigits x  
    | x < 10 = [x] 
    | otherwise = [mod x 10] ++ (toDigits $ divideByTen x)