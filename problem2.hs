main = putStrLn (show (fib 4000000))

fib :: Int -> Int
fib limit = fib' 0 1 limit 0

fib' :: Int -> Int -> Int -> Int -> Int
fib' a b limit acc
  | c > limit = acc
  | otherwise = fib' b c limit newacc
  where c = a + b
        is_even = c `mod` 2 == 0
        newacc = if is_even then acc + c else acc
