import Utils

main :: IO ()
main = do
  file <- readFile "input/013.txt"
  let nums = map (read :: String -> Integer) $ lines file
  let toNum = fst . (foldr (\c (x,y) -> (x+c*10^y, y+1)) (0,0))
  print $ toNum $ take 10 $ getDigits $ sum nums
