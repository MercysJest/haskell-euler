import NumberTheory
import Data.List
import Data.Ord
import qualified Data.IntSet as IS

quad :: Int -> Int -> Int -> Int
quad !a !b !n = n*n + a*n + b

cntPrimes :: Int -> (Int, Int) -> Int
cntPrimes !n (!a, !b) = if mRabin (abs (quad a b n))
                        then cntPrimes (n+1) (a,b)
                        else n

main :: IO ()
main = print $ (\((x,y),_) -> x*y) $ maximumBy (comparing snd) $ map (\x -> (x, cntPrimes 2 x)) poss
  where poss :: [(Int, Int)]
        poss = [(a,b) | a <- [-999,-997..999], b <- takeWhile (<=1000) primes]
        -- for n = 0, b has to be prime. for n = 1, a has to be odd
