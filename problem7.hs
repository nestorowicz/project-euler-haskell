-- quite slow
main = putStrLn (show (findNthPrime 10001))

findNthPrime = findNextPrime [] 2

findNextPrime :: [Int] -> Int -> Int -> Int
findNextPrime primes i limit
  | length primes == limit = head primes
  | all (\x -> i `mod` x /= 0) primes = findNextPrime (i:primes) (i+1) limit
  | otherwise = findNextPrime primes (i+1) limit
