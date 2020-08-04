main = putStrLn (show largestPalindrome)

largestPalindrome = maximum (filter (isPalindrome) products)

products = concat (map (\x -> map (*x) [999, 998 .. x]) [999, 998 .. 1])

isPalindrome :: Integer -> Bool
isPalindrome n = (n - reversed n) == 0
  where
    reversed n = reversedAcc 0 n
    reversedAcc :: Integer -> Integer -> Integer
    reversedAcc acc n
      | n == 0 = acc
      | otherwise = reversedAcc (acc * 10 + n `mod` 10) (n `div` 10)

