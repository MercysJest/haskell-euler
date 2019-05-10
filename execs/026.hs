import NumberTheory
import Data.Ord
import Data.List

-- https://math.stackexchange.com/questions/2442809/how-can-i-get-the-length-of-repeating-decimal

main :: IO ()
main = print $ fst $ maximumBy (comparing snd) cands
  where strip25 :: Int -> Int
        strip25 = (until odd (`div` 2)) . (until (\x->x`mod`5 /= 0) (`div` 5))
        cands = [(v, getOrd 10 (strip25 v)) | v <- [2..999], strip25 v /= 1]
