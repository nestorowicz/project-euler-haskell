main = putStrLn (show (findTriplet 1 2))

findTriplet :: Int -> Int -> Int
findTriplet a b
  | isSquareNumber && isPythagoreanTriplet && (a + b + c == 1000) = a * b * c
  | (a + b + c) > 1000 = findTriplet (a + 1) (a + 2)
  | otherwise = findTriplet a (b + 1)
  where
    csquared = (a * a + b * b)
    c = floor (sqrt (fromIntegral csquared))
    isSquareNumber = (c * c) == csquared
    isPythagoreanTriplet = (a < b && b < c) && (a * a + b * b == c * c)
