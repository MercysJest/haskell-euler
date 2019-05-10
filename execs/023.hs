import NumberTheory
import Utils
import Data.List
import Data.IntSet (fromList, member)

main :: IO ()
main = print $ sum' $ filter (\x -> not $ member x sums) [1..28123]
  where isAb n =  n < (sum' . getProperDivisors) n
        abnums = filter isAb [1..28123]
        sums = fromList [x+y | (x:xs) <- tails abnums, y <-x:xs]
