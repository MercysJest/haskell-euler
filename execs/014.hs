import Data.List
import Data.Ord

colSeq :: Int -> [Int]
colSeq = (takeWhile ((/=) 1)) . (iterate step)
  where step n = if even n then n `div` 2 else 3*n+1

main :: IO ()
main = print $ snd $ maximumBy (comparing fst) [((length . colSeq) n, n) | n <- [2..10^6]]
