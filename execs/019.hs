import Utils
-- (d, dm, m, y)

isEndY dm m = m == 12 && dm == 31

isLeapY y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

isEndM dm m y
  | m `elem` [1, 3, 5, 7, 8, 10, 12] = dm == 31
  | m `elem` [4,6,9,11] = dm == 30
  | otherwise = if isLeapY y then dm == 29 else dm == 28

step :: Integral a => (a,a,a,a) -> (a,a,a,a)
step (d, dm, m, y)
  | isEndY dm m = (incD d, 1, 1, y+1)
  | isEndM dm m y = (incD d, 1, m + 1, y)
  | otherwise = (incD d, dm + 1, m, y)
    where incD day = (day+1) `mod` 7

--filter isEndM takeUntil iterate (==(0, 31, 12, 2000))

main :: IO ()
main = print $ length $ filter (\(d, dm,_,_)-> d == 0 && dm == 1) days 
  where lst (_,dm,m,y) = dm == 31 && m == 12 && y == 2000
        days = takeUntil lst $ iterate step (2,1,1,1901)
