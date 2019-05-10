import Utils
import Data.List
import Data.List.Split
import Data.Char

main :: IO ()
main = do
  file <- readFile "input/022.txt"
  let names = sort $ splitOn "," $ filter (/='"') file
  print $ sum' $ map (\(x,y) -> x*y) $ zip [1..] (map score names)
    where score name = sum $ map ((subtract 64) . ord) name
