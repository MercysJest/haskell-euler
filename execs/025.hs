import NumberTheory
import Utils
import Data.List

main :: IO ()
main = print $ find ((>=10^999) . snd) fibs
  where step (x,y) = (y, x+y)
        fibs = zip[1..] $ map fst $ iterate step (1,1)
