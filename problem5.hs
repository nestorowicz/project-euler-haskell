import Data.List
main = putStrLn (show (makeDivisibleList 1 [1..20]))

makeDivisibleList :: Int -> [Int] -> Int
makeDivisibleList n xs = foldl (\x acc -> makeDivisible acc x) n xs

makeDivisible :: Int -> Int -> Int
makeDivisible a b = foldl (*) a ((factors b) \\ (factors a))

factors :: Int -> [Int]
factors n 
  | n < 1 = error "Negative numbers not supported"
  | otherwise = findFactors 2 n
  where
    findFactors :: Int -> Int -> [Int]
    findFactors i n
      | i >= n = [n]
      | n `mod` i == 0 = i:(findFactors i (n `div` i))
      | otherwise = findFactors (i+1) n

