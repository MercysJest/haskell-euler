module Utils where

-- list of digits backwards
getDigits :: Integral a => a -> [a]
getDigits !n 
  | n < 10 = [n]
  | otherwise = n `mod` 10:getDigits (n `div` 10)
