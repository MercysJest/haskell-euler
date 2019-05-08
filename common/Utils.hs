module Utils where

import Data.List

-- list of digits
getDigits :: Integral a => a -> [a]
getDigits = reverse . dig 
  where dig !n
          | n < 10 = [n]
          | otherwise = n `mod` 10:dig (n `div` 10)

getMatrix :: String -> [[Integer]]
getMatrix str = map ((map (read :: String -> Integer)) . words) $ lines str

takeUntil :: (a -> Bool) -> [a] -> [a] 
takeUntil _ [] = []
takeUntil !f (x:xs) = x:if f x then [] else takeUntil f xs

sum' :: Num a => [a] -> a
sum' l = foldl' (+) 0 l

product' :: Num a => [a] -> a
product' l = foldl' (*) 1 l
