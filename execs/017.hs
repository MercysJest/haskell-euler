import Utils 

ones d = case d of 1 -> "one"
                   2 -> "two"
                   3 -> "three"
                   4 -> "four"
                   5 -> "five"
                   6 -> "six"
                   7 -> "seven"
                   8 -> "eight"
                   9 -> "nine"
                   _ -> ""

tens d = case d of 2 -> "twenty"
                   3 -> "thirty"
                   4 -> "forty"
                   5 -> "fifty"
                   6 -> "sixty"
                   7 -> "seventy"
                   8 -> "eighty"
                   9 -> "ninety"
                   _ -> ""

tenCase d = case d of 10 -> "ten"
                      11 -> "eleven"
                      12 -> "twelve"
                      13 -> "thirteen"
                      14 -> "fourteen"
                      15 -> "fifteen"
                      16 -> "sixteen"
                      17 -> "seventeen"
                      18 -> "eighteen"
                      19 -> "nineteen"

lt100 n
  | n < 10 = ones n
  | n < 20 = tenCase n 
  | otherwise = tens (dig !! 0) ++ (ones $ dig !! 1)
    where dig = getDigits n

toWords n
  | n == 1000 = "onethousand"
  | n < 100 = lt100 n
  | n `mod` 100 == 0 = hund
  | otherwise = hund ++ "and" ++ (lt100 ((dig !! 1)*10 + (dig !! 2)))
    where dig = getDigits n
          hund = (ones $ dig !! 0) ++ "hundred"
          

main :: IO ()
main = print $ sum $ map (length . toWords) [1..1000]
