import Data.List

main :: IO ()
main = print $ foldl' lcm 1 [1..20]
