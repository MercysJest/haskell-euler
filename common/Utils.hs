module Utils where

-- list of digits
getDigits :: Integral a => a -> [a]
getDigits = reverse . dig 
  where dig !n
          | n < 10 = [n]
          | otherwise = n `mod` 10:dig (n `div` 10)
