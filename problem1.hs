-- Multiples of 3 and 5
--
sum' :: Int -> Int -> Int
sum' multiplier limit = truncate result
  where first = 1
        max = limit - 1
        last = fromIntegral (max `div` multiplier)
        result = (first + last) / 2 * last * fromIntegral multiplier

main = putStrLn ("Answer: " ++ show (sum' 3 1000 + sum' 5 1000 - sum' 15 1000))
