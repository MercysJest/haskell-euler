fib :: [Int]
fib = 1:2:(zipWith (+) fib (tail fib))

main :: IO ()
main = print $ sum $ takeWhile (< 4*10^6) $ filter even fib
