doubleOrSelf :: (Integral a) => a -> Bool -> a 
doubleOrSelf x y
    | y == True = x * 2
    | y == False = x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [doubleOrSelf x True]
doubleEveryOther (x:xs) 
    | isLenthOdd = [doubleOrSelf x True] ++ (doubleEveryOther $ xs)
    | otherwise = [doubleOrSelf x False] ++ (doubleEveryOther $ xs)
    where isLenthOdd = (length (x:xs)) `mod` 2 /= 0