--Break an Integer into [Integer]
divideByTen :: Integer -> Integer
divideByTen = floor . realToFrac . (/10) . fromIntegral

toDigits :: Integer -> [Integer]
toDigits x  
    | x < 10 = [x] 
    | otherwise = (toDigits $ divideByTen x) ++ [mod x 10]