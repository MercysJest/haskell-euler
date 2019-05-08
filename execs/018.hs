import Data.List 

-- 3
-- 7 4
-- 2 4 6
-- 8 5 9 3
-- (0, 2), (1, 1), (2, 6)
--    0         1         2         3
-- [(8, [8]), (5, [5]), (9, [9]), (3, [3])]

step :: Integral a => [(a, [a])] -> [a] -> [(a, [a])]
step acc row = map choice (zip [0..] row)
  where pair idx = (acc !! idx, acc !! (idx+1))
        choice (idx, val) =
          let ((val1, l1),(val2, l2)) = pair idx in
            if val1 > val2 then (val1+val, val:l1)
                           else (val2+val, val:l2)

main :: IO ()
main = do
  file <- readFile "input/018.txt"
  let pyramid = reverse $ map ((map (read :: String -> Integer)) . words) $ lines file
  let step0 = map (\x -> (x,[x])) $ head pyramid
  print $ foldl' step step0 (tail pyramid)
