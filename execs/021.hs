import NumberTheory
import Utils
import Data.List
import Data.Maybe
import qualified Data.IntMap.Strict as IM

isAmPair :: (IM.IntMap Int) -> (Int, Int) -> Bool
isAmPair sMap (x,y) = sX == y && sY == x
  where sX = fromJust $ IM.lookup x sMap
        sY = fromJust $ IM.lookup y sMap

-- maybe do precomputation to get map of n to d(n)
main :: IO ()
main = print $ foldl' coll 0 ams
  where sMap = foldl' (\acc x -> IM.insert x (sum' $ init $ getDivisors x) acc) IM.empty [1..9999]
        ams = filter (isAmPair sMap) [(x,y) | (x:xs) <- tails [1..9999], y <- xs]
        coll acc (x,y) = acc+x+y
