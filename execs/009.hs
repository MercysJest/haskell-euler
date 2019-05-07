import Data.List

-- Euclid's Formula
triple :: Integral a => (a,a) -> (a,a,a)
triple (m,n) = (m^2-n^2, 2*m*n, m^2+n^2)

-- m^2-n^2+2mn+m^2+n^2 = 2m^2 + 2mn
-- 2m^2 + 2mn = 1000 => m(m+n) = 500
-- => m | 500 and n = 500 - m
main :: IO ()
main = print $ (\(a,b,c) -> a*b*c) $ head triples 
  where div500 = filter (\x -> 500 `mod` x == 0) [1..499]
        mns = [(m,n) | m <- div500, let n = (500 `div` m) - m, n > 0, m^2-n^2 > 0]
        triples = map triple mns
