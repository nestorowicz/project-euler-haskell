main = putStrLn (show (maxPrimeFactor 600851475143))


maxPrimeFactor :: Integer -> Integer
maxPrimeFactor n
  | n < 2 = error "Given number is not composite"
  | otherwise = maxPrimeFactor' 2 2 n

maxPrimeFactor' :: Integer -> Integer -> Integer -> Integer
maxPrimeFactor' i lastPrime n
  | n `mod` i == 0 = maxPrimeFactor' i i (n `div` i)
  | i >= n = lastPrime
  | otherwise = maxPrimeFactor' (i+1) lastPrime n
