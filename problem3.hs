main = putStrLn (show (maxFactor 600851475143))

maxFactor limit = maxFactor' 2 limit
maxFactor' :: Integer -> Integer -> Integer
maxFactor' i n
  | i >= n = n
  | n `mod` i == 0 = maxFactor' i (n `div` i)
  | otherwise = maxFactor' (i + 1) n

