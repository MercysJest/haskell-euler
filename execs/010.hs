import NumberTheory
import Data.List

main :: IO ()
main = print $ foldl' (+) 0 $ takeWhile (<2*10^6) primes
