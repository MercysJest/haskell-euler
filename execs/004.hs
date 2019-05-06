import Utils
import Data.List

isPD :: Int -> Bool
isPD n = dig == reverse dig
  where dig = getDigits n

main :: IO ()
main = print $ maximum $ filter isPD prods
  where prods = [x*y | (x:xs) <- tails [100..999], y <- xs]
