import NumberTheory
import Data.List

main :: IO ()
main = print $ head $ filter (\x -> numDiv x > 500) tris
  where numDiv =  length . nub . subsequences . trialFactor
        tris :: [Int]
        tris = map fst (iterate (\(x,y) -> (x+y,y+1)) (1,2))
