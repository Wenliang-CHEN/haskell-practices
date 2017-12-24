module Main where

divideByTen :: Integer -> Integer
divideByTen = floor . realToFrac . (/10) . fromIntegral

toDigits :: Integer -> [Integer]
toDigits x  
    | x < 10 = [x] 
    | otherwise = [mod x 10] ++ (toDigits $ divideByTen x)

getDoubledSecondDigits :: [Integer] -> [Integer]
getDoubledSecondDigits [] = []
getDoubledSecondDigits [x] = [x*2]
getDoubledSecondDigits (x:xs) 
    | isLengthOdd = [(x*2)] ++ (getDoubledSecondDigits $ xs)
    | otherwise = [x] ++ (getDoubledSecondDigits $ xs)
    where isLengthOdd = (length (x:xs)) `mod` 2 /= 0

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum $ toDigits x
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs 

validateCreditCardNumber :: Integer -> Bool
validateCreditCardNumber x
    | length digits /= 16 = False
    | otherwise = (sumDigits $ getDoubledSecondDigits $ toDigits x) `mod` 10 == 0
    where digits = toDigits x

main :: IO ()
main = do  
    putStrLn "key in an integer"  
    digit <- getLine  
    print $ validateCreditCardNumber $ read digit
  
--main = print $ getDoubledSecondDigits $ toDigits 0000000000002220
