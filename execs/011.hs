import Data.List

lateralMax :: Integral a => [[a]] -> a
lateralMax = maximum . (map product) . groups
  where groups = concatMap ((filter (\x -> length x == 4)) . (map (take 4)) . tails)

diagMax :: Integral a => [[a]] -> a
diagMax matrix = lateralMax diags
  where rend = length matrix
        cend = length $ head matrix
        bound (x,y) = x == cend || y == rend
        coords (x,y) = takeWhile (not . bound) $ iterate (\(a,b) -> (a+1, b+1)) (x,y)
        item (x,y) = (matrix !! y) !! x
        diags = map (map item) [coords (x,0) | x <- [0..rend-4]]

main :: IO ()
main = do
  file <- readFile "input/011.txt"
  let matrix = map ((map (read :: String -> Integer)) . words) $ lines file
  print $ maximum [lateralMax matrix, lateralMax (transpose matrix), diagMax matrix, diagMax (map reverse matrix)]
